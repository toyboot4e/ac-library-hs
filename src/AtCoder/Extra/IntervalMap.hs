{-# LANGUAGE DerivingStrategies #-}

-- original implementation:
-- <https://noimi.hatenablog.com/entry/2021/05/02/195143>

-- | A dense map that manages non-overlapping intervals \((l, r, x)\) with the range \([0, n)\).
-- Interval state changes can be tracked using the @onAdd@ and @onDel@ hooks.
--
-- ==== Invariant
-- Each interval is operated as a whole, similar to a persistant data structure. When part of an
-- inerval is modified, the whole interval is deleted first, and the subintervals are re-inserted.
-- It's important for tracking non-linear interval information with the @onAdd@ and @onDel@ hooks
-- (callbacks).
--
-- ==== __Example__
-- Create an `IntervalMap` that covers a half-open interval \([0, n)\):
--
-- >>> import AtCoder.Extra.IntervalMap qualified as ITM
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> import Data.Vector.Unboxed.Mutable qualified as VUM
-- >>> itm <- ITM.new @_ @Int 4
--
-- It handles range set queries in amortized \(O(\log n)\) time:
--
-- >>> ITM.insert itm 0 4 0 -- 0 0 0 0
-- >>> ITM.insert itm 1 3 1 -- 0 1 1 0
-- >>> ITM.freeze itm
-- [(0,(1,0)),(1,(3,1)),(3,(4,0))]
--
-- Track interval informations with the @onAdd@ and @onDel@ hooks:
--
-- >>> import Debug.Trace (traceShow)
-- >>> itm <- ITM.new @_ @Int 4
-- >>> let onAdd l r x = print ("onAdd", l, r, x)
-- >>> let onDel l r x = print ("onDel", l, r, x)
--
-- >>> ITM.insertM itm 0 4 0 onAdd onDel -- 0 0 0 0
-- ("onAdd",0,4,0)
--
-- >>> ITM.insertM itm 1 3 1 onAdd onDel -- 0 1 1 0
-- ("onDel",0,4,0)
-- ("onAdd",0,1,0)
-- ("onAdd",3,4,0)
-- ("onAdd",1,3,1)
--
-- >>> ITM.deleteM itm 0 4 onAdd onDel
-- ("onDel",0,1,0)
-- ("onDel",1,3,1)
-- ("onDel",3,4,0)
--
-- @since 1.1.0.0
module AtCoder.Extra.IntervalMap
  ( -- * IntervalMap
    IntervalMap,

    -- * Constructors
    new,
    build,
    buildM,

    -- * Metadata
    capacity,

    -- * Lookups
    contains,
    intersects,
    lookup,
    read,
    readMaybe,

    -- * Modifications

    -- ** Inserting
    insert,
    insertM,

    -- ** Deleting
    delete,
    deleteM,

    -- ** Overwriting
    overwrite,
    overwriteM,

    -- * Conversions
    freeze,
  )
where

import AtCoder.Extra.IntMap qualified as IM
import Control.Monad (foldM_)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Vector.Generic qualified as G
import Data.Vector.Unboxed qualified as VU
import GHC.Stack (HasCallStack)
import Prelude hiding (lookup, read)

-- | A dense map that manages non-overlapping intervals \((l, r, x)\) with the range \([0, n)\).
-- Interval state changes can be tracked using the @onAdd@ and @onDel@ hooks.
--
-- @since 1.1.0.0
newtype IntervalMap s a = IntervalMap
  { -- | @l@ -> @(r, a)@
    unITM :: IM.IntMap s (Int, a)
  }

-- | \(O(n)\) Creates an empty `IntervalMap`.
--
-- @since 1.1.0.0
new :: (PrimMonad m, VU.Unbox a) => Int -> m (IntervalMap (PrimState m) a)
new = fmap IntervalMap . IM.new

-- | \(O(n + m \log n)\) Creates an `IntervalMap` by combining consecutive equal values into one
-- interval.
--
-- ==== __Example__
-- >>> itm <- build @_ @Int (VU.fromList [10,10,11,11,12,12])
-- >>> freeze itm
-- [(0,(2,10)),(2,(4,11)),(4,(6,12))]
--
-- @since 1.1.0.0
build :: (PrimMonad m, Eq a, VU.Unbox a) => VU.Vector a -> m (IntervalMap (PrimState m) a)
build xs = buildM xs onAdd
  where
    onAdd _ _ _ = pure ()

-- | \(O(n + m \log n)\) Creates an `IntervalMap` by combining consecutive equal values into one
-- interval, while performing @onAdd@ hook for each interval.
--
-- @since 1.1.0.0
buildM ::
  (PrimMonad m, Eq a, VU.Unbox a) =>
  VU.Vector a ->
  (Int -> Int -> a -> m ()) ->
  m (IntervalMap (PrimState m) a)
buildM xs onAdd = do
  dim <- IM.new (G.length xs)
  foldM_ (step dim) (0 :: Int) $ G.group xs
  pure $ IntervalMap dim
  where
    step dim !l !xs' = do
      let !l' = l + G.length xs'
      IM.insert dim l (l', G.head xs')
      onAdd l l' (G.head xs')
      pure l'

-- | \(O(1)\) Returns the capacity \(n\), where the interval \([0, n)\) is managed by the map.
--
-- @since 1.1.0.0
{-# INLINE capacity #-}
capacity :: IntervalMap s a -> Int
capacity = IM.capacity . unITM

-- | \(O(\log n)\) Point variant of `intersects`.
--
-- @since 1.1.0.0
contains :: (PrimMonad m, VU.Unbox a) => IntervalMap (PrimState m) a -> Int -> m Bool
contains itm i = intersects itm i (i + 1)

-- | \(O(\log n)\) Boolean variant of `lookup`.
--
-- @since 1.1.0.0
intersects :: (PrimMonad m, VU.Unbox a) => IntervalMap (PrimState m) a -> Int -> Int -> m Bool
intersects (IntervalMap dim) l r
  | l >= r = pure False
  | otherwise = do
      res <- IM.lookupLE dim l
      pure $ case res of
        Just (!_, (!r', !_)) -> r <= r'
        _ -> False

-- | \(O(\log n)\) Looks up an interval that contains \([l, r)\).
--
-- @since 1.1.0.0
lookup :: (PrimMonad m, VU.Unbox a) => IntervalMap (PrimState m) a -> Int -> Int -> m (Maybe (Int, Int, a))
lookup (IntervalMap im) l r
  | l >= r = pure Nothing
  | otherwise = do
      res <- IM.lookupLE im l
      pure $ case res of
        Just (!l', (!r', !a))
          | r <= r' -> Just (l', r', a)
        _ -> Nothing

-- | \(O(\log n)\) Looks up an interval that contains \([l, r)\) and reads out the value.
--
-- @since 1.1.0.0
read :: (HasCallStack, PrimMonad m, VU.Unbox a) => IntervalMap (PrimState m) a -> Int -> Int -> m a
read itm l r = do
  res <- readMaybe itm l r
  pure $ case res of
    Just !a -> a
    Nothing -> error $ "[read] not a member: " ++ show (l, r)

-- | \(O(\log n)\) Looks up an interval that contains \([l, r)\) and reads out the value.
--
-- @since 1.1.0.0
readMaybe :: (PrimMonad m, VU.Unbox a) => IntervalMap (PrimState m) a -> Int -> Int -> m (Maybe a)
readMaybe (IntervalMap dim) l r
  | l >= r = pure Nothing
  | otherwise = do
      res <- IM.lookupLE dim l
      pure $ case res of
        Just (!_, (!r', !a))
          | r <= r' -> Just a
        _ -> Nothing

-- | Amortized \(O(\log n)\) interval insertion. Overlapping intervals are overwritten.
--
-- @since 1.1.0.0
insert :: (PrimMonad m, Eq a, VU.Unbox a) => IntervalMap (PrimState m) a -> Int -> Int -> a -> m ()
insert itm l r x = insertM itm l r x onAdd onDel
  where
    onAdd _ _ _ = pure ()
    onDel _ _ _ = pure ()

-- | Amortized \(O(\log n)\) interval insertion with side effects via @onAdd@ and @onDel@ hooks.
-- Overlapping intervals are overwritten.
--
-- @since 1.1.0.0
insertM ::
  (PrimMonad m, Eq a, VU.Unbox a) =>
  IntervalMap (PrimState m) a ->
  Int ->
  Int ->
  a ->
  (Int -> Int -> a -> m ()) ->
  (Int -> Int -> a -> m ()) ->
  m ()
insertM (IntervalMap dim) l0 r0 x onAdd onDel
  | l0 >= r0 = pure ()
  | otherwise = do
      !r <- handleRight l0 r0
      (!l', !r') <- handleLeft l0 r
      onAdd l' r' x
      IM.insert dim l' (r', x)
  where
    handleRight l r = do
      res <- IM.lookupGE dim l
      case res of
        Just interval0@(!_, (!_, !_)) -> run interval0 l r
        Nothing -> pure r

    -- Looks into intervals with @l' >= l0@.
    --           [----]
    -- (i)            *--------]   overwrite if it's x
    -- (ii)   [-------]*      delete anyways
    -- (iii)    *(------]     overwrite if it's x, or
    run (!l', (!r', !x')) l r
      | l' > r = do
          -- not adjacent: end.
          pure r
      -- (i)
      | l' == r && x' == x = do
          -- adjacent interval with the same value: merge into one.
          onDel l' r' x'
          IM.delete_ dim l'
          pure r'
      | l' == r = do
          -- adjacent interval with different values: nothing to do.
          pure r
      -- (ii)
      | r' <= r = do
          -- inside the interval: delete and continue
          onDel l' r' x'
          IM.delete_ dim l'
          res <- IM.lookupGT dim l'
          case res of
            Just rng -> run rng l r
            Nothing -> pure r
      -- (iii)
      | x' == x = do
          -- intersecting interval with the same value: merge into one.
          onDel l' r' x'
          IM.delete_ dim l'
          pure r'
      | otherwise = do
          -- intersecting interval with a different value: delete the intersection.
          onDel l' r' x'
          onAdd r r' x'
          IM.delete_ dim l'
          IM.insert dim r (r', x')
          pure r

    handleLeft l r = do
      res <- IM.lookupLT dim l
      case res of
        Nothing -> pure (l, r)
        Just (!l', (!r', !x'))
          -- (i): adjacent interval
          | r' == l && x' == x -> do
              -- adjacent interval with the same value: merge into one.
              onDel l' r' x'
              IM.delete_ dim l'
              pure (l', r)
          | r' == l -> do
              -- adjacent interval with different values: nothing to do.
              pure (l, r)
          -- (ii): not adjacent or intersecting
          | r' < l -> do
              pure (l, r)
          -- (iii): intersecting
          | x' == x -> do
              -- insersecting interval with the same value: merge into one.
              onDel l' r' x'
              IM.delete_ dim l'
              pure (min l l', max r r')
          | r' > r -> do
              -- [l', r') contains [l, r) with a different value: split into three.
              onDel l' r' x'
              onAdd l' l x'
              onAdd r r' x'
              -- IM.delete_ dim l'
              IM.insert dim l' (l, x')
              IM.insert dim r (r', x')
              pure (l, r)
          | otherwise -> do
              -- insersecting interval with a different value: delete.
              onDel l' r' x'
              onAdd l' l x'
              -- IM.delete_ dim l'
              IM.insert dim l' (l, x')
              pure (l, r)

-- | Amortized \(O(\log n)\) interval deletion.
--
-- @since 1.1.0.0
delete :: (PrimMonad m, VU.Unbox a) => IntervalMap (PrimState m) a -> Int -> Int -> m ()
delete itm l r = deleteM itm l r onAdd onDel
  where
    onAdd _ _ _ = pure ()
    onDel _ _ _ = pure ()

-- | Amortized \(O(\log n)\) interval deletion with side effects via @onAdd@ and @onDel@ hooks.
--
-- @since 1.1.0.0
deleteM ::
  (PrimMonad m, VU.Unbox a) =>
  IntervalMap (PrimState m) a ->
  Int ->
  Int ->
  (Int -> Int -> a -> m ()) ->
  (Int -> Int -> a -> m ()) ->
  m ()
deleteM (IntervalMap dim) l0 r0 onAdd onDel
  | l0 >= r0 = pure ()
  | otherwise = do
      handleRight l0 r0
      handleLeft l0 r0
  where
    handleRight l r = do
      res <- IM.lookupGE dim l
      case res of
        Just interval0@(!_, (!_, !_)) -> run interval0 l r
        Nothing -> pure ()

    run (!l', (!r', !x')) l r
      | l' >= r = do
          -- not intersecting
          pure ()
      | r' <= r = do
          -- contained
          onDel l' r' x'
          IM.delete_ dim l'
          res <- IM.lookupGT dim l'
          case res of
            Just rng -> run rng l r
            Nothing -> pure ()
      | otherwise = do
          -- intersecting
          onDel l' r' x'
          onAdd r r' x'
          IM.delete_ dim l'
          IM.insert dim r (r', x')
          pure ()

    handleLeft l r = do
      res <- IM.lookupLT dim l
      case res of
        Nothing -> pure ()
        Just (!l', (!r', !x'))
          | r' <= l -> do
              -- not intersecting
              pure ()
          | r' > r -> do
              -- [l', r') contains [l, r)
              onDel l' r' x'
              onAdd l' l x'
              onAdd r r' x'
              -- IM.delete dim l'
              IM.insert dim l' (l, x')
              IM.insert dim r (r', x')
          | otherwise -> do
              -- intersecting
              onDel l' r' x'
              onAdd l' l x'
              -- IM.delete_ dim l'
              IM.insert dim l' (l, x')

-- | \(O(\log n)\) Shorthand for overwriting the value of an interval that contains \([l, r)\).
--
-- @since 1.1.0.0
overwrite :: (PrimMonad m, Eq a, VU.Unbox a) => IntervalMap (PrimState m) a -> Int -> Int -> a -> m ()
overwrite itm l r x = do
  res <- lookup itm l r
  case res of
    Just (!l', !r', !_) -> insert itm l' r' x
    Nothing -> pure ()

-- | \(O(\log n)\). Shorthand for overwriting the value of an interval that contains \([l, r)\).
--
-- @since 1.1.0.0
overwriteM ::
  (PrimMonad m, Eq a, VU.Unbox a) =>
  IntervalMap (PrimState m) a ->
  Int ->
  Int ->
  a ->
  (Int -> Int -> a -> m ()) ->
  (Int -> Int -> a -> m ()) ->
  m ()
overwriteM itm l r x onAdd onDel = do
  res <- lookup itm l r
  case res of
    Just (!l', !r', !_) -> insertM itm l' r' x onAdd onDel
    Nothing -> pure ()

-- | \(O(n \log n)\) Enumerates the intervals as \((l, (r, x))\) tuples, where \([l, r)\) is the
-- interval and \(x\) is the associated value.
--
-- @since 1.1.0.0
freeze :: (PrimMonad m, VU.Unbox a) => IntervalMap (PrimState m) a -> m (VU.Vector (Int, (Int, a)))
freeze = IM.assocs . unITM