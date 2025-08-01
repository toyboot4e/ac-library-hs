{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- | Key-value pairs with monoid products and monoid actions on them through the `SegAct` instance.
--
-- ==== Performance
-- This module is __extremely slow__ as an ordinary map. Do not use it unless you need monoid
-- products.
--
-- ==== __Example__
--
-- >>> import AtCoder.Extra.Monoid.RangeAdd qualified as RangeAdd
-- >>> import AtCoder.Extra.Seq.Map qualified as M
-- >>> import Data.Semigroup (Sum (..))
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> m <- M.new @_ @(RangeAdd.RangeAdd (Sum Int)) @Int @(Sum Int) 10
-- >>> M.insert m 1 10 -- [- 10 - - -]
-- >>> M.insert m 3 30 -- [- 10 - 30 -]
-- >>> M.prod m 1 2
-- Sum {getSum = 10}
--
-- >>> M.applyIn m 1 4 $ RangeAdd.new 7 -- [-  17 - 37 -]
-- >>> M.prod m 1 4
-- Sum {getSum = 54}
--
-- @since 1.2.1.0
module AtCoder.Extra.Seq.Map
  ( -- * Map
    Map (..),

    -- * Re-exports
    SegAct (..),

    -- * Constructors
    new,
    build,
    reset,

    -- * Metadata
    capacity,
    size,

    -- * Key-based operations

    -- ** Read/write
    member,
    lookup,
    adjust,

    -- ** Insert/delete
    insert,
    insertWith,
    delete,
    delete_,

    -- ** Products

    -- sliceST,
    prod,
    prodMaybe,
    allProd,

    -- ** Applications
    applyIn,
    applyAll,

    -- ** Bisection methods
    lookupLE,
    lookupLT,
    lookupGE,
    lookupGT,

    -- * Index-based operations

    -- ** Read/write
    readAt,
    readMaybeAt,
    writeAt,
    modifyAt,
    exchangeAt,

    -- ** Products
    prodInInterval,
    -- TODO: prodIn

    -- ** Applications
    applyInInterval,

    -- ** Bisection methods
    ilowerBound,
    ilowerBoundM,
    ilowerBoundProd,
    ilowerBoundProdM,

    -- * Conversion
    freeze,
  )
where

import AtCoder.Extra.Pool qualified as P
import AtCoder.Extra.Seq qualified as Seq
import AtCoder.Extra.Seq.Raw qualified as Raw
import AtCoder.Internal.Assert qualified as ACIA
import AtCoder.LazySegTree (SegAct (..))
import Control.Monad (unless, when)
import Control.Monad.Primitive (PrimMonad, PrimState, stToPrim)
import Control.Monad.ST (ST)
import Data.Coerce (coerce)
import Data.Ord (comparing)
import Data.Vector.Algorithms.Intro qualified as VAI
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Stack (HasCallStack)
import Prelude hiding (lookup, read, reverse, seq)

-- | Key-value pairs with monoid products and monoid actions on them through the `SegAct` instance.
--
-- @since 1.2.1.0
data Map s f k v = Map
  { -- | The sequence storage.
    --
    -- @since 1.2.1.0
    seqMap :: !(Seq.Seq s f v),
    -- | Keys.
    --
    -- @since 1.2.1.0
    kMap :: !(VUM.MVector s k),
    -- | Handle of the root node.
    --
    -- @since 1.2.1.0
    rootMap :: !(Seq.Handle s)
  }

{-# INLINE assertRootST #-}
assertRootST :: (HasCallStack) => Raw.Seq s f v -> P.Index -> ST s ()
assertRootST Seq.Seq {pSeq} i = do
  p <- VGM.read pSeq (coerce i)
  let !_ = ACIA.runtimeAssert (P.nullIndex p) $ "AtCoder.Extra.Seq.Map.assertRootST: not a root (node `" ++ show i ++ "`, parent `" ++ show p ++ "`)"
  pure ()

-- | \(O(n)\) Creates a new `Map` of capacity \(n\). Always prefer `build` to `new` for better
-- performance.
--
-- @since 1.2.1.0
{-# INLINE new #-}
new :: (PrimMonad m, Monoid f, VU.Unbox f, VU.Unbox k, VU.Unbox v, Monoid v) => Int -> m (Map (PrimState m) f k v)
new n = stToPrim $ do
  seqMap <- Seq.new n
  kMap <- VUM.unsafeNew n
  rootMap <- P.newHandle P.undefIndex
  pure Map {..}

-- | \(O(n \log n)\) Creates a new `Map` of capacity \(n\) with initial values. Always prefer `build` to
-- `new` for better performance.
--
-- @since 1.2.1.0
{-# INLINEABLE build #-}
build :: (HasCallStack, PrimMonad m, Monoid f, VU.Unbox f, Ord k, VU.Unbox k, VU.Unbox v, Monoid v) => Int -> VU.Vector (k, v) -> m (Map (PrimState m) f k v)
build n kvs = stToPrim $ do
  -- let !_ = ACIA.runtimeAssert (VU.length kvs <= n) "AtCoder.Extra.Seq.Map"
  seqMap <- Seq.new n
  kMap <- VUM.unsafeNew n
  -- note that `unzip` is O(1) for tuples:
  let (!ks, !vs) = VU.unzip $ VU.modify (VAI.sortBy (comparing fst)) kvs
  VU.iforM_ ks $ VGM.write kMap
  rootMap <- Seq.newSeq seqMap vs
  pure Map {..}

-- | \(O(1)\) Clears the map. All the handles must not be used again.
--
-- @since 1.2.1.0
{-# INLINEABLE reset #-}
reset :: (PrimMonad m, Monoid f, VU.Unbox f, VU.Unbox k, VU.Unbox v, Monoid v) => Map (PrimState m) f k v -> m ()
reset Map {..} = stToPrim $ do
  Raw.resetST seqMap
  VGM.write (Seq.unHandle rootMap) 0 P.undefIndex

-- -------------------------------------------------------------------------------------------
-- Metadata
-- -------------------------------------------------------------------------------------------

-- | \(O(1)\) Returns the maximum number of elements the map can store.
--
-- @since 1.2.1.0
{-# INLINEABLE capacity #-}
capacity :: Map s f k v -> Int
capacity Map {seqMap} = Raw.capacity seqMap

-- | \(O(1)\) Returns the number of elements in the map.
--
-- @since 1.2.1.0
{-# INLINEABLE size #-}
size :: (PrimMonad m) => Map (PrimState m) f k v -> m Int
size Map {..} = stToPrim $ do
  root <- VGM.read (Seq.unHandle rootMap) 0
  Raw.lengthST seqMap root

-- -------------------------------------------------------------------------------------------
-- Key-based operations
-- -------------------------------------------------------------------------------------------

-- | Amortized \(O(\log n)\). Finds a node with key \(k\).
{-# INLINEABLE lookupNodeST #-}
lookupNodeST :: (HasCallStack, Eq f, Monoid f, VU.Unbox f, Ord k, VU.Unbox k, Monoid v, VU.Unbox v, SegAct f v) => Map s f k v -> k -> ST s (Bool, P.Index, P.Index)
lookupNodeST Map {..} k = do
  root <- VGM.read (Seq.unHandle rootMap) 0
  if P.nullIndex root
    then pure (False, P.undefIndex, P.undefIndex)
    else do
      (!l, !root') <- Raw.maxRightWithST seqMap root $ \i -> do
        ki <- VGM.read kMap (coerce i)
        pure $ ki <= k
      VGM.write (Seq.unHandle rootMap) 0 root'
      if P.nullIndex l
        then do
          pure (False, root', root')
        else do
          kl <- VGM.read kMap (coerce l)
          pure (kl == k, l, root')

-- | Amoritzed \(O(\log n)\). Returns whether a node with key \(k\) is in the map.
--
-- @since 1.2.1.0
{-# INLINE member #-}
member :: (HasCallStack, PrimMonad m, Eq f, Monoid f, VU.Unbox f, Ord k, VU.Unbox k, Monoid v, VU.Unbox v, SegAct f v) => Map (PrimState m) f k v -> k -> m Bool
member m k = stToPrim $ do
  (!b, !_, !_) <- lookupNodeST m k
  pure b

-- | Amortized \(O(\log n)\). Looks up for the monoid value of a node with key \(k\).
--
-- @since 1.2.1.0
{-# INLINE lookup #-}
lookup :: (HasCallStack, PrimMonad m, Eq f, Monoid f, VU.Unbox f, Ord k, VU.Unbox k, Monoid v, VU.Unbox v, SegAct f v) => Map (PrimState m) f k v -> k -> m (Maybe v)
lookup m@Map {..} k = stToPrim $ do
  (!b, !l, !_) <- lookupNodeST m k
  if b
    then do
      Raw.splayST seqMap l True
      Just <$> VGM.read (Seq.vSeq seqMap) 0
    else do
      pure Nothing

-- | Amoritzed \(O(\log n)\). Adjusts the monoid value of a node with key \(k\).
--
-- @since 1.2.1.0
{-# INLINE adjust #-}
adjust :: (HasCallStack, PrimMonad m, Eq f, Monoid f, VU.Unbox f, Ord k, VU.Unbox k, Monoid v, VU.Unbox v, SegAct f v) => Map (PrimState m) f k v -> (v -> v) -> k -> m ()
adjust m@Map {..} f k = stToPrim $ do
  (!b, !l, !_) <- lookupNodeST m k
  when b $ do
    Raw.splayST seqMap l True
    VGM.write (Seq.unHandle rootMap) 0 l
    Raw.modifyNodeST seqMap f l

-- | Amortized \(O(\log n)\). Inserts a \((k, v)\) pair. If the key is already present in the map,
-- the associated value is replaced with the supplied value.
--
-- @since 1.2.1.0
{-# INLINE insert #-}
insert :: (HasCallStack, PrimMonad m, Eq f, Monoid f, VU.Unbox f, Ord k, VU.Unbox k, Monoid v, VU.Unbox v, SegAct f v) => Map (PrimState m) f k v -> k -> v -> m ()
insert m k v = stToPrim $ do
  insertWithST m const k v

-- | Amortized \(O(\log n)\). Inserts a \((k, v)\) pairs, combining new value and old value.
--
-- @since 1.2.1.0
{-# INLINE insertWith #-}
insertWith ::
  (HasCallStack, PrimMonad m, Eq f, Monoid f, VU.Unbox f, Ord k, VU.Unbox k, Monoid v, VU.Unbox v, SegAct f v) =>
  -- | Map.
  Map (PrimState m) f k v ->
  -- | new -> old -> combined.
  (v -> v -> v) ->
  -- | Key.
  k ->
  -- | Value.
  v ->
  m ()
insertWith m f k v = stToPrim $ do
  insertWithST m f k v

-- | Amortized \(O(\log n)\).
{-# INLINEABLE insertWithST #-}
insertWithST ::
  (HasCallStack, Eq f, Monoid f, VU.Unbox f, Ord k, VU.Unbox k, Monoid v, VU.Unbox v, SegAct f v) =>
  -- | Map.
  Map s f k v ->
  -- | new -> old -> combined.
  (v -> v -> v) ->
  -- | Key.
  k ->
  -- | Value.
  v ->
  ST s ()
insertWithST Map {..} f k v = stToPrim $ do
  -- split and merge
  VGM.unsafeModifyM
    (Seq.unHandle rootMap)
    ( \root -> do
        (!l, !r) <- Raw.splitMaxRightWithST seqMap root $ \i -> do
          ki <- VGM.read kMap (coerce i)
          pure $ ki <= k
        if P.nullIndex l
          then do
            -- insert
            node <- Raw.newNodeST seqMap v
            VGM.write kMap (coerce node) k
            Raw.mergeST seqMap node r
          else do
            kl <- VGM.read kMap (coerce l)
            if kl == k
              then do
                -- overwrite the node
                Raw.splayST seqMap l True
                Raw.modifyNodeST seqMap (f v) l
                VGM.write kMap (coerce l) k
                Raw.mergeST seqMap l r
              else do
                -- insert
                node <- Raw.newNodeST seqMap v
                VGM.write kMap (coerce node) k
                Raw.merge3ST seqMap l node r
    )
    0

-- | Amortized \(O(\log n)\). Deletes an element with key \(k\).
--
-- @since 1.2.1.0
{-# INLINEABLE delete #-}
delete :: (HasCallStack, PrimMonad m, Eq f, Monoid f, VU.Unbox f, Ord k, VU.Unbox k, Monoid v, VU.Unbox v, SegAct f v) => Map (PrimState m) f k v -> k -> m (Maybe v)
delete m@Map {..} k = stToPrim $ do
  (!b, !l, !_) <- lookupNodeST m k
  if b
    then do
      let Raw.Seq {..} = seqMap
      Raw.splayST seqMap l True
      xl <- VGM.read lSeq $ coerce l
      xr <- VGM.read rSeq $ coerce l
      unless (P.nullIndex xl) $ VGM.write pSeq (coerce xl) P.undefIndex
      unless (P.nullIndex xr) $ VGM.write pSeq (coerce xr) P.undefIndex
      v <- VGM.read vSeq $ coerce l
      Raw.freeNodeST seqMap l
      root'' <- Raw.mergeST seqMap xl xr
      VGM.write (Seq.unHandle rootMap) 0 root''
      pure $ Just v
    else do
      pure Nothing

-- | Amortized \(O(\log n)\). Deletes an element with key \(k\).
--
-- @since 1.2.1.0
{-# INLINE delete_ #-}
delete_ :: (HasCallStack, PrimMonad m, Eq f, Monoid f, VU.Unbox f, Ord k, VU.Unbox k, Monoid v, VU.Unbox v, SegAct f v) => Map (PrimState m) f k v -> k -> m ()
delete_ m k = stToPrim $ do
  _ <- delete m k
  pure ()

-- | Amortized \(O(\log n)\). Captures a node that corresponds to \([k1, k2)\).
{-# INLINEABLE sliceST #-}
sliceST :: (HasCallStack, Eq f, Monoid f, VU.Unbox f, Ord k, VU.Unbox k, Monoid v, VU.Unbox v, SegAct f v) => Map s f k v -> k -> k -> ST s P.Index
sliceST Map {..} k1 k2 = do
  let handle = Seq.unHandle rootMap
  root <- VGM.read handle 0
  if P.nullIndex root
    then pure P.undefIndex
    else do
      (!lm, !r) <- Raw.splitMaxRightWithST seqMap root $ \i -> do
        k' <- VGM.read kMap (coerce i)
        pure $! k' < k2

      case (P.nullIndex lm, P.nullIndex r) of
        (True, True) -> error "unreachable"
        (True, False) -> do
          VGM.write handle 0 r
          pure P.undefIndex
        (False, True) -> do
          (!l, !root') <- Raw.maxRightWithST seqMap lm $ \i -> do
            k' <- VGM.read kMap (coerce i)
            pure $! k' < k1
          if P.nullIndex l
            then do
              VGM.write handle 0 root'
              pure root'
            else do
              Raw.splayST seqMap l True
              VGM.write handle 0 l
              VGM.read rSeq (coerce l)
        (False, False) -> do
          r' <- Raw.splayKthST seqMap r 0
          VGM.write handle 0 r'
          (!l, !root') <- Raw.maxRightWithST seqMap lm $ \i -> do
            k' <- VGM.read kMap (coerce i)
            pure $! k' < k1
          if P.nullIndex l
            then do
              -- root' is [l, r)
              VGM.write pSeq (coerce root') r'
              VGM.write lSeq (coerce r') root'
              Raw.updateNodeST seqMap root'
              pure root'
            else do
              -- o--l--o--r--o
              --          r
              --     /---/
              --    l
              --     \
              --      m
              Raw.splayST seqMap l True
              VGM.write pSeq (coerce l) r'
              VGM.write lSeq (coerce r') l
              Raw.updateNodeST seqMap r'
              VGM.read rSeq (coerce l)
  where
    Raw.Seq {..} = seqMap

-- | Amortized \(O(\log n)\). Returns the monoid product in an interval \([k_1, k_2)\). Throws an
-- error for \(k_1 \gt k_2\).
--
-- ==== Constraints
-- - \(k_1 \le k_2\)
--
-- @since 1.2.1.0
{-# INLINE prod #-}
prod :: (HasCallStack, PrimMonad m, Eq f, Monoid f, VU.Unbox f, Ord k, VU.Unbox k, Monoid v, VU.Unbox v, SegAct f v) => Map (PrimState m) f k v -> k -> k -> m v
prod m@Map {..} l r = stToPrim $ do
  let !_ = ACIA.runtimeAssert (l <= r) "AtCoder.Extra.Seq.Map.prod: k1 > k2"
  root <- VGM.read (Seq.unHandle rootMap) 0
  if P.nullIndex root || l == r
    then pure mempty
    else unsafeProdST m l r

-- | Amortized \(O(\log n)\). Returns the monoid product in an interval \([k_1, k_2)\). Returns
-- `Nothing` if an invalid interval is given or for an empty sequence.
--
-- @since 1.2.1.0
{-# INLINE prodMaybe #-}
prodMaybe :: (HasCallStack, PrimMonad m, Eq f, Monoid f, VU.Unbox f, Ord k, VU.Unbox k, Monoid v, VU.Unbox v, SegAct f v) => Map (PrimState m) f k v -> k -> k -> m (Maybe v)
prodMaybe m l r
  | l > r = pure Nothing
  | otherwise = Just <$> prod m l r

-- | Amortized \(O(\log n)\). Returns the monoid product in an interval \([k_1, k_2)\). Returns
-- `Nothing` if an invalid interval is given or for an empty sequence.
--
-- @since 1.2.1.0
{-# INLINE allProd #-}
allProd :: (HasCallStack, PrimMonad m, Eq f, Monoid f, VU.Unbox f, Ord k, VU.Unbox k, Monoid v, VU.Unbox v, SegAct f v) => Map (PrimState m) f k v -> m v
allProd Map {..} = do
  root <- VGM.read (Seq.unHandle rootMap) 0
  if P.nullIndex root
    then pure mempty
    else VGM.read (Raw.prodSeq seqMap) (coerce root)

-- | Amortized \(O(\log n)\).
--
-- ==== Constraint
-- - \(0 \le \lt r \le n\). Note that the interval must have positive length.
{-# INLINEABLE unsafeProdST #-}
unsafeProdST :: (HasCallStack, Eq f, Monoid f, VU.Unbox f, Ord k, VU.Unbox k, Monoid v, VU.Unbox v, SegAct f v) => Map s f k v -> k -> k -> ST s v
unsafeProdST m@Map {..} l r = do
  let Seq.Seq {..} = seqMap
  root <- VGM.read (Seq.unHandle rootMap) 0
  assertRootST seqMap root -- TODO: remove
  target <- sliceST m l r
  if P.nullIndex target
    then pure mempty
    else do
      res <- VGM.read prodSeq $ coerce target
      Raw.splayST seqMap target True
      VGM.write (Seq.unHandle rootMap) 0 target
      pure res

-- | Amortized \(O(\log n)\). Given an interval \([l, r)\), applies a monoid action \(f\).
--
-- ==== Constraints
-- - \(0 \le l \le r \le n\)
-- - The root must point a non-empty sequence.
--
-- @since 1.2.1.0
{-# INLINEABLE applyIn #-}
applyIn :: (HasCallStack, PrimMonad m, Eq f, Monoid f, VU.Unbox f, Ord k, VU.Unbox k, Monoid v, VU.Unbox v, SegAct f v) => Map (PrimState m) f k v -> k -> k -> f -> m ()
applyIn m@Map {..} l r act = stToPrim $ do
  let !_ = ACIA.runtimeAssert (l <= r) "AtCoder.Extra.Seq.Map.applyIn: k1 > k2"
  unless (l == r) $ do
    target <- sliceST m l r
    unless (P.nullIndex target) $ do
      Raw.applyNodeST seqMap target act
      Raw.splayST seqMap target True
      VGM.write (Seq.unHandle rootMap) 0 target

-- | Amortized \(O(\log n)\). Applies a monoid action \(f\) to every element.
--
-- @since 1.2.1.0
{-# INLINE applyAll #-}
applyAll :: (HasCallStack, PrimMonad m, Eq f, Monoid f, VU.Unbox f, Ord k, VU.Unbox k, Monoid v, VU.Unbox v, SegAct f v) => Map (PrimState m) f k v -> f -> m ()
applyAll Map {..} act = stToPrim $ do
  root <- VGM.read (Seq.unHandle rootMap) 0
  Raw.applyToRootST seqMap root act

-- -------------------------------------------------------------------------------------------
-- Key-based bisection method
-- -------------------------------------------------------------------------------------------

-- | Amortized \(O(\log n)\). Looks up for \((k, v)\) pair with the maximum key \(k\) such that
-- \(k \le k_{\mathrm{ref}}\).
--
-- @since 1.2.1.0
{-# INLINE lookupLE #-}
lookupLE :: (HasCallStack, PrimMonad m, Eq f, Monoid f, VU.Unbox f, Ord k, VU.Unbox k, Monoid v, VU.Unbox v, SegAct f v) => Map (PrimState m) f k v -> k -> m (Maybe (k, v))
lookupLE m k = stToPrim $ do
  lookupImplL m k EQ

-- | Amortized \(O(\log n)\). Looks up for \((k, v)\) pair with the maximum key \(k\) such that
-- \(k \lt k_{\mathrm{ref}}\).
--
-- @since 1.2.1.0
{-# INLINE lookupLT #-}
lookupLT :: (HasCallStack, PrimMonad m, Eq f, Monoid f, VU.Unbox f, Ord k, VU.Unbox k, Monoid v, VU.Unbox v, SegAct f v) => Map (PrimState m) f k v -> k -> m (Maybe (k, v))
lookupLT m k = stToPrim $ do
  lookupImplL m k LT

-- | Amortized \(O(\log n)\).
{-# INLINEABLE lookupImplL #-}
lookupImplL :: (HasCallStack, Eq f, Monoid f, VU.Unbox f, Ord k, VU.Unbox k, Monoid v, VU.Unbox v, SegAct f v) => Map s f k v -> k -> Ordering -> ST s (Maybe (k, v))
lookupImplL Map {..} k o = do
  root <- VGM.read (Seq.unHandle rootMap) 0
  if P.nullIndex root
    then pure Nothing
    else do
      (!l, !root') <- Raw.maxRightWithST seqMap root $ \i -> do
        ki <- VGM.read kMap (coerce i)
        pure $! compare ki k <= o
      if P.nullIndex l
        then do
          VGM.write (Seq.unHandle rootMap) 0 root'
          pure Nothing
        else do
          Raw.splayST seqMap l True -- TODO: is it True?
          VGM.write (Seq.unHandle rootMap) 0 l
          kl <- VGM.read kMap (coerce l)
          vl <- VGM.read (Seq.vSeq seqMap) (coerce l)
          pure $! Just (kl, vl)

-- | Amortized \(O(\log n)\). Looks up for \((k, v)\) pair with the minimum key \(k\) such that
-- \(k \ge k_{\mathrm{ref}}\).
--
-- @since 1.2.1.0
{-# INLINE lookupGE #-}
lookupGE :: (HasCallStack, PrimMonad m, Eq f, Monoid f, VU.Unbox f, Ord k, VU.Unbox k, Monoid v, VU.Unbox v, SegAct f v) => Map (PrimState m) f k v -> k -> m (Maybe (k, v))
lookupGE m k = stToPrim $ do
  lookupImplR m k LT

-- | Amortized \(O(\log n)\). Looks up for \((k, v)\) pair with the minimum key \(k\) such that
-- \(k \gt k_{\mathrm{ref}}\).
--
-- @since 1.2.1.0
{-# INLINE lookupGT #-}
lookupGT :: (HasCallStack, PrimMonad m, Eq f, Monoid f, VU.Unbox f, Ord k, VU.Unbox k, Monoid v, VU.Unbox v, SegAct f v) => Map (PrimState m) f k v -> k -> m (Maybe (k, v))
lookupGT m k = stToPrim $ do
  lookupImplR m k EQ

-- | Amortized \(O(\log n)\).
{-# INLINEABLE lookupImplR #-}
lookupImplR :: (HasCallStack, Eq f, Monoid f, VU.Unbox f, Ord k, VU.Unbox k, Monoid v, VU.Unbox v, SegAct f v) => Map s f k v -> k -> Ordering -> ST s (Maybe (k, v))
lookupImplR Map {..} k o = do
  let handle = Seq.unHandle rootMap
  root <- VGM.read handle 0
  if P.nullIndex root
    then pure Nothing
    else do
      let Raw.Seq {..} = seqMap
      (!l, !root') <- Raw.maxRightWithST seqMap root $ \i -> do
        k' <- VGM.read kMap (coerce i)
        pure $! compare k' k <= o

      if P.nullIndex l
        then do
          r <- Raw.splayKthST seqMap (coerce root') 0
          VGM.write handle 0 r
          kr <- VGM.read kMap (coerce r)
          vr <- VGM.read vSeq (coerce r)
          pure $ Just (kr, vr)
        else do
          Raw.splayST seqMap l True -- TODO: is it `True`?
          r0 <- VGM.read rSeq (coerce l)
          if P.nullIndex r0
            then do
              VGM.write handle 0 l
              pure Nothing
            else do
              r <- Raw.splayKthST seqMap (coerce r0) 0
              VGM.write handle 0 r
              kr <- VGM.read kMap (coerce r)
              vr <- VGM.read vSeq (coerce r)
              pure $ Just (kr, vr)

-- -------------------------------------------------------------------------------------------
-- Index-based operations
-- -------------------------------------------------------------------------------------------

-- | Amortized \(O(\log n)\). Reads the \(k\)-th node's monoid value.
--
-- @since 1.2.1.0
{-# INLINE readAt #-}
readAt :: (HasCallStack, PrimMonad m, Eq f, Monoid f, VU.Unbox f, Monoid v, VU.Unbox v, SegAct f v) => Map (PrimState m) f k v -> Int -> m v
readAt Map {..} i = stToPrim $ do
  Seq.read seqMap rootMap i

-- | Amortized \(O(\log n)\). Reads the \(k\)-th node's monoid value.
--
-- @since 1.2.1.0
{-# INLINE readMaybeAt #-}
readMaybeAt :: (HasCallStack, PrimMonad m, Eq f, Monoid f, VU.Unbox f, Monoid v, VU.Unbox v, SegAct f v) => Map (PrimState m) f k v -> Int -> m (Maybe v)
readMaybeAt Map {..} i = stToPrim $ do
  Seq.readMaybe seqMap rootMap i

-- | Amortized \(O(\log n)\). Writes to the \(k\)-th node's monoid value.
--
-- @since 1.2.1.0
{-# INLINE writeAt #-}
writeAt :: (HasCallStack, PrimMonad m, Eq f, Monoid f, VU.Unbox f, Monoid v, VU.Unbox v, SegAct f v) => Map (PrimState m) f k v -> Int -> v -> m ()
writeAt Map {..} i v = stToPrim $ do
  Seq.write seqMap rootMap i v

-- | Amortized \(O(\log n)\). Given a user function \(f\), modifies the \(k\)-th node's monoid value
-- with it.
--
-- @since 1.2.1.0
{-# INLINE modifyAt #-}
modifyAt :: (HasCallStack, PrimMonad m, Eq f, Monoid f, VU.Unbox f, Monoid v, VU.Unbox v, SegAct f v) => Map (PrimState m) f k v -> (v -> v) -> Int -> m ()
modifyAt Map {..} f i = stToPrim $ do
  Seq.modify seqMap rootMap f i

-- | Amortized \(O(\log n)\). Exchanges the \(k\)-th node's monoid value.
--
-- @since 1.2.1.0
{-# INLINE exchangeAt #-}
exchangeAt :: (HasCallStack, PrimMonad m, Eq f, Monoid f, VU.Unbox f, Monoid v, VU.Unbox v, SegAct f v) => Map (PrimState m) f k v -> Int -> v -> m v
exchangeAt Map {..} i v = stToPrim $ do
  Seq.exchange seqMap rootMap i v

-- | Amortized \(O(\log n)\). Returns the monoid product in an interval \([l, r)\).
--
-- @since 1.2.1.0
{-# INLINE prodInInterval #-}
prodInInterval :: (HasCallStack, PrimMonad m, Eq f, Monoid f, VU.Unbox f, Monoid v, VU.Unbox v, SegAct f v) => Map (PrimState m) f k v -> Int -> Int -> m v
prodInInterval Map {..} l r = stToPrim $ do
  Seq.prod seqMap rootMap l r

-- | Amortized \(O(\log n)\). Given an interval \([l, r)\), applies a monoid action \(f\) to it.
--
-- @since 1.2.1.0
{-# INLINE applyInInterval #-}
applyInInterval :: (HasCallStack, PrimMonad m, Eq f, Monoid f, VU.Unbox f, Monoid v, VU.Unbox v, SegAct f v) => Map (PrimState m) f k v -> Int -> Int -> f -> m ()
applyInInterval Map {..} l r f = stToPrim $ do
  Seq.applyIn seqMap rootMap l r f

-- | Amortized \(O(\log n)\).
--
-- @since 1.2.1.0
{-# INLINE ilowerBound #-}
ilowerBound ::
  (HasCallStack, PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) =>
  -- | Map.
  Map (PrimState m) f k a ->
  -- | User predicate \(f(i, v_i)\) that takes the index and the monoid value.
  (Int -> a -> Bool) ->
  -- | Maximum \(r\), where \(f(i, v_i)\) holds for \(i \in [0, r)\).
  m Int
ilowerBound Map {..} f = stToPrim $ do
  Seq.ilowerBound seqMap rootMap f

-- | Amortized \(O(\log n)\).
--
-- @since 1.2.1.0
{-# INLINE ilowerBoundM #-}
ilowerBoundM ::
  (HasCallStack, PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) =>
  -- | Map.
  Map (PrimState m) f k a ->
  -- | User predicate \(f(i, v_i)\) that takes the index and the monoid value.
  (Int -> a -> m Bool) ->
  -- | Maximum \(r\), where \(f(i, v_i)\) holds for \(i \in [0, r)\).
  m Int
ilowerBoundM Map {..} f = do
  Seq.ilowerBoundM seqMap rootMap f

-- | Amortized \(O(\log n)\).
--
-- @since 1.2.1.0
{-# INLINE ilowerBoundProd #-}
ilowerBoundProd ::
  (HasCallStack, PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) =>
  -- | Map.
  Map (PrimState m) f k a ->
  -- | User predicate \(f(i, \Pi_{0 \le j \le i} v_j)\) that takes the index and the monoid product.
  (Int -> a -> Bool) ->
  -- | Maximum \(r\), where \(f(i, \Pi_{0 \le j \le i} v_j)\) holds for \(i \in [0, r)\).
  m Int
ilowerBoundProd Map {..} f = stToPrim $ do
  Seq.ilowerBoundProd seqMap rootMap f

-- | Amortized \(O(\log n)\).
--
-- @since 1.2.1.0
{-# INLINE ilowerBoundProdM #-}
ilowerBoundProdM ::
  (HasCallStack, PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) =>
  -- | Map.
  Map (PrimState m) f k a ->
  -- | User predicate \(f(i, \Pi_{0 \le j \le i} v_j)\) that takes the index and the monoid product.
  (Int -> a -> m Bool) ->
  -- | Maximum \(r\), where \(f(i, \Pi_{0 \le j \le i} v_j)\) holds for \(i \in [0, r)\).
  m Int
ilowerBoundProdM Map {..} f = do
  Seq.ilowerBoundProdM seqMap rootMap f

-- -------------------------------------------------------------------------------------------
-- Conversions
-- -------------------------------------------------------------------------------------------

-- | \(O(n)\) Returns the \(k, v\) pairs in the map.
--
-- @since 1.2.1.0
{-# INLINEABLE freeze #-}
freeze :: (HasCallStack, PrimMonad m, Eq f, Monoid f, VU.Unbox f, Ord k, VU.Unbox k, Monoid v, VU.Unbox v, SegAct f v) => Map (PrimState m) f k v -> m (VU.Vector (k, v))
freeze Map {..} = stToPrim $ do
  let Raw.Seq {..} = seqMap
  root0 <- VGM.read (Seq.unHandle rootMap) 0
  if P.nullIndex root0
    then pure VU.empty
    else do
      assertRootST seqMap root0
      size_ <- VGM.read sSeq (coerce root0)
      res <- VUM.unsafeNew size_
      let inner i root
            | P.nullIndex root = pure i
            | otherwise = do
                -- visit from left to right
                Raw.propNodeST seqMap root
                i' <- inner i =<< VGM.read lSeq (coerce root)
                kx <- VGM.read kMap (coerce root)
                vx <- VGM.read vSeq (coerce root)
                VGM.write res i' (kx, vx)
                inner (i' + 1) =<< VGM.read rSeq (coerce root)
      _ <- inner 0 root0
      VU.unsafeFreeze res
