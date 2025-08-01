{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- | A dense, fast `Int` map implemented as a 64-ary tree that covers an interval \([0, n)\).
--
-- ==== __Example__
-- Create an `IntMap` with capacity \(10\):
--
-- >>> import AtCoder.Extra.IntMap qualified as IM
-- >>> im <- IM.new @_ @Int 10
--
-- `insert`, `delete`, `lookup`, `lookupGT` and other functions are available:
--
-- >>> IM.insert im 0 100
-- >>> IM.insert im 9 101
-- >>> IM.delete im 0
-- True
--
-- >>> IM.size im
-- 1
--
-- >>> IM.lookup im 9
-- Just 101
--
-- >>> IM.lookup im 1
-- Nothing
--
-- >>> IM.lookupGT im 5
-- Just (9,101)
--
-- @since 1.1.0.0
module AtCoder.Extra.IntMap
  ( -- * IntMap
    IntMap,

    -- * Constructors
    new,
    build,

    -- * Metadata
    capacity,
    size,
    null,

    -- * Lookups
    lookup,
    member,
    notMember,

    -- ** Comparative lookups
    lookupGE,
    lookupGT,
    lookupLE,
    lookupLT,

    -- ** Max/Min lookups
    lookupMin,
    lookupMax,

    -- * Modifications

    -- ** Insertions
    insert,
    insertWith,

    -- ** Updates
    modify,
    modifyM,

    -- ** Deletions
    delete,
    delete_,
    deleteMin,
    deleteMax,

    -- * Conversions
    keys,
    elems,
    assocs,
  )
where

import AtCoder.Extra.IntSet qualified as IS
import Control.Monad (when)
import Control.Monad.Primitive (PrimMonad, PrimState, stToPrim)
import Control.Monad.ST (ST)
import Data.Maybe (fromJust)
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Stack (HasCallStack)
import Prelude hiding (lookup, null)

-- | A dense, fast `Int` map implemented as a 64-ary tree that covers an interval \([0, n)\).
--
-- @since 1.1.0.0
data IntMap s a = IntMap
  { setIM :: !(IS.IntSet s),
    valIM :: !(VUM.MVector s a)
  }

-- | \(O(n)\) Creates an `IntMap` for an interval \([0, n)\).
--
-- @since 1.1.0.0
{-# INLINE new #-}
new :: (PrimMonad m, VU.Unbox a) => Int -> m (IntMap (PrimState m) a)
new cap = stToPrim $ newST cap

-- | \(O(n + m \log n)\) Creates an `IntMap` for an interval \([0, n)\) with initial entries.
--
-- @since 1.1.0.0
{-# INLINE build #-}
build :: (PrimMonad m, VU.Unbox a) => Int -> VU.Vector (Int, a) -> m (IntMap (PrimState m) a)
build cap xs = stToPrim $ buildST cap xs

-- | \(O(1)\) Returns the capacity \(n\), where the interval \([0, n)\) is covered by the map.
--
-- @since 1.1.0.0
{-# INLINE capacity #-}
capacity :: IntMap s a -> Int
capacity = IS.capacity . setIM

-- | \(O(1)\) Returns the number of entries in the map.
--
-- @since 1.1.0.0
{-# INLINE size #-}
size :: (PrimMonad m) => IntMap (PrimState m) a -> m Int
size = IS.size . setIM

-- | \(O(1)\) Returns whether the map is empty.
--
-- @since 1.1.0.0
{-# INLINE null #-}
null :: (PrimMonad m) => IntMap (PrimState m) a -> m Bool
null = IS.null . setIM

-- | \(O(\log n)\) Looks up the value associated with a key.
--
-- @since 1.1.0.0
{-# INLINE lookup #-}
lookup :: (PrimMonad m, VU.Unbox a) => IntMap (PrimState m) a -> Int -> m (Maybe a)
lookup im k = stToPrim $ lookupST im k

-- | \(O(\log n)\) Tests whether a key \(k\) is in the map.
--
-- @since 1.1.0.0
{-# INLINE member #-}
member :: (PrimMonad m) => IntMap (PrimState m) a -> Int -> m Bool
member = IS.member . setIM

-- | \(O(\log n)\) Tests whether a key \(k\) is not in the map.
--
-- @since 1.1.0.0
{-# INLINE notMember #-}
notMember :: (PrimMonad m) => IntMap (PrimState m) a -> Int -> m Bool
notMember = IS.notMember . setIM

-- | \(O(\log n)\) Looks up the \((k, v)\) pair with the smallest key \(k\) such that \(k \ge k_0\).
--
-- @since 1.1.0.0
{-# INLINE lookupGE #-}
lookupGE :: (PrimMonad m, VU.Unbox a) => IntMap (PrimState m) a -> Int -> m (Maybe (Int, a))
lookupGE im k = stToPrim $ lookupGEST im k

-- | \(O(\log n)\) Looks up the \((k, v)\) pair with the smallest \(k\) such that \(k \gt k_0\).
--
-- @since 1.1.0.0
{-# INLINE lookupGT #-}
lookupGT :: (PrimMonad m, VU.Unbox a) => IntMap (PrimState m) a -> Int -> m (Maybe (Int, a))
lookupGT is k = stToPrim $ lookupGEST is (k + 1)

-- | \(O(\log n)\) Looks up the \((k, v)\) pair with the largest key \(k\) such that \(k \le k_0\).
--
-- @since 1.1.0.0
{-# INLINE lookupLE #-}
lookupLE :: (HasCallStack, PrimMonad m, VU.Unbox a) => IntMap (PrimState m) a -> Int -> m (Maybe (Int, a))
lookupLE im k = stToPrim $ lookupLEST im k

-- | \(O(\log n)\) Looks up the \((k, v)\) pair with the largest key \(k\) such that \(k \lt k_0\).
--
-- @since 1.1.0.0
{-# INLINE lookupLT #-}
lookupLT :: (PrimMonad m, VU.Unbox a) => IntMap (PrimState m) a -> Int -> m (Maybe (Int, a))
lookupLT im k = stToPrim $ lookupLEST im (k - 1)

-- | \(O(\log n)\) Looks up the \((k, v)\) pair with the minimum key \(k\).
--
-- @since 1.1.0.0
{-# INLINE lookupMin #-}
lookupMin :: (PrimMonad m, VU.Unbox a) => IntMap (PrimState m) a -> m (Maybe (Int, a))
lookupMin im = stToPrim $ lookupMinST im

-- | \(O(\log n)\) Looks up the \((k, v)\) pair with the maximum key \(k\).
--
-- @since 1.1.0.0
{-# INLINE lookupMax #-}
lookupMax :: (PrimMonad m, VU.Unbox a) => IntMap (PrimState m) a -> m (Maybe (Int, a))
lookupMax im = stToPrim $ lookupMaxST im

-- | \(O(\log n)\) Inserts a \((k, v)\) pair into the map. If an entry with the same key already
-- exists, it is overwritten.
--
-- @since 1.1.0.0
{-# INLINE insert #-}
insert :: (HasCallStack, PrimMonad m, VU.Unbox a) => IntMap (PrimState m) a -> Int -> a -> m ()
insert im k v = stToPrim $ insertST im k v

-- | \(O(\log n)\) Inserts a \((k, v)\) pair into the map. If an entry with the same key already
-- exists, it overwritten with \(f(v_{\mathrm{new}}, v_{\mathrm{old}})\).
--
-- @since 1.1.0.0
{-# INLINE insertWith #-}
insertWith :: (HasCallStack, PrimMonad m, VU.Unbox a) => IntMap (PrimState m) a -> (a -> a -> a) -> Int -> a -> m ()
insertWith im f k v = stToPrim $ insertWithST im f k v

-- | \(O(\log n)\) Modifies the value associated with a key. If an entry with the same key already
-- does not exist, it does nothing.
--
-- @since 1.1.0.0
{-# INLINE modify #-}
modify :: (HasCallStack, PrimMonad m, VU.Unbox a) => IntMap (PrimState m) a -> (a -> a) -> Int -> m ()
modify im f k = stToPrim $ modifyST im f k

-- | \(O(\log n)\) Modifies the value associated with a key. If an entry with the same key already
-- does not exist, it does nothing.
--
-- @since 1.1.0.0
{-# INLINE modifyM #-}
modifyM :: (HasCallStack, PrimMonad m, VU.Unbox a) => IntMap (PrimState m) a -> (a -> m a) -> Int -> m ()
modifyM IntMap {..} f k = do
  b <- IS.member setIM k
  when b $ do
    VGM.modifyM valIM f k

-- | \(O(\log n)\) Deletes the \((k, v)\) pair with the key \(k\) from the map. Does nothing if no
-- such key exists. Returns whether the key existed.
--
-- @since 1.1.0.0
{-# INLINE delete #-}
delete :: (PrimMonad m) => IntMap (PrimState m) a -> Int -> m Bool
delete im = stToPrim . deleteST im

-- | \(O(\log n)\) Deletes the \((k, v)\) pair with the key \(k\) from the map. Does nothing if no
-- such key exists.
--
-- @since 1.1.0.0
{-# INLINE delete_ #-}
delete_ :: (PrimMonad m) => IntMap (PrimState m) a -> Int -> m ()
delete_ im = stToPrim . deleteST_ im

-- | \(O(\log n)\) Deletes the \((k, v)\) pair with the minimum key \(k\) in the map.
--
-- @since 1.1.0.0
{-# INLINE deleteMin #-}
deleteMin :: (HasCallStack, PrimMonad m, VU.Unbox a) => IntMap (PrimState m) a -> m (Maybe (Int, a))
deleteMin is = stToPrim $ deleteMinST is

-- | \(O(\log n)\) Deletes the \((k, v)\) pair with maximum key \(k\) in the map.
--
-- @since 1.1.0.0
{-# INLINE deleteMax #-}
deleteMax :: (HasCallStack, PrimMonad m, VU.Unbox a) => IntMap (PrimState m) a -> m (Maybe (Int, a))
deleteMax is = stToPrim $ deleteMaxST is

-- | \(O(n \log n)\) Enumerates the keys in the map.
--
-- @since 1.1.0.0
{-# INLINE keys #-}
keys :: (PrimMonad m) => IntMap (PrimState m) a -> m (VU.Vector Int)
keys = stToPrim . keysST

-- | \(O(n \log n)\) Enumerates the elements (values) in the map.
--
-- @since 1.1.0.0
{-# INLINE elems #-}
elems :: (PrimMonad m, VU.Unbox a) => IntMap (PrimState m) a -> m (VU.Vector a)
elems = stToPrim . elemsST

-- | \(O(n \log n)\) Enumerates the key-value pairs in the map.
--
-- @since 1.1.0.0
{-# INLINE assocs #-}
assocs :: (PrimMonad m, VU.Unbox a) => IntMap (PrimState m) a -> m (VU.Vector (Int, a))
assocs = stToPrim . assocsST

-- -------------------------------------------------------------------------------------------------
-- Internal
-- -------------------------------------------------------------------------------------------------

{-# INLINEABLE newST #-}
newST :: (VU.Unbox a) => Int -> ST s (IntMap s a)
newST cap = do
  setIM <- IS.new cap
  valIM <- VUM.unsafeNew cap
  pure IntMap {..}

{-# INLINEABLE buildST #-}
buildST :: (VU.Unbox a) => Int -> VU.Vector (Int, a) -> ST s (IntMap s a)
buildST cap xs = do
  im <- new cap
  VU.forM_ xs $ \(!k, !v) -> do
    insertST im k v
  pure im

-- | \(O(\log n)\) Looks up the value for a key.
--
-- @since 1.1.0.0
{-# INLINEABLE lookupST #-}
lookupST :: (VU.Unbox a) => IntMap s a -> Int -> ST s (Maybe a)
lookupST im@IntMap {..} k = do
  member im k >>= \case
    True -> Just <$> VGM.read valIM k
    False -> pure Nothing

{-# INLINEABLE lookupGEST #-}
lookupGEST :: (VU.Unbox a) => IntMap s a -> Int -> ST s (Maybe (Int, a))
lookupGEST IntMap {..} k = do
  IS.lookupGE setIM k >>= \case
    Just i -> Just . (i,) <$> VGM.read valIM i
    Nothing -> pure Nothing

{-# INLINEABLE lookupLEST #-}
lookupLEST :: (HasCallStack, VU.Unbox a) => IntMap s a -> Int -> ST s (Maybe (Int, a))
lookupLEST IntMap {..} k = do
  IS.lookupLE setIM k >>= \case
    Just i -> Just . (i,) <$> VGM.read valIM i
    Nothing -> pure Nothing

{-# INLINEABLE lookupMinST #-}
lookupMinST :: (VU.Unbox a) => IntMap s a -> ST s (Maybe (Int, a))
lookupMinST is = lookupGEST is 0

{-# INLINEABLE lookupMaxST #-}
lookupMaxST :: (VU.Unbox a) => IntMap s a -> ST s (Maybe (Int, a))
lookupMaxST im = lookupLEST im (IS.capacity (setIM im) - 1)

{-# INLINEABLE insertST #-}
insertST :: (HasCallStack, VU.Unbox a) => IntMap s a -> Int -> a -> ST s ()
insertST IntMap {..} k v = do
  IS.insert setIM k
  VGM.write valIM k v

{-# INLINEABLE insertWithST #-}
insertWithST :: (HasCallStack, VU.Unbox a) => IntMap s a -> (a -> a -> a) -> Int -> a -> ST s ()
insertWithST IntMap {..} f k v = do
  b <- IS.member setIM k
  if b
    then do
      VGM.modify valIM (f v) k
    else do
      IS.insert setIM k
      VGM.write valIM k v

{-# INLINEABLE modifyST #-}
modifyST :: (HasCallStack, VU.Unbox a) => IntMap s a -> (a -> a) -> Int -> ST s ()
modifyST IntMap {..} f k = do
  b <- IS.member setIM k
  when b $ do
    VGM.modify valIM f k

{-# INLINEABLE deleteST #-}
deleteST :: IntMap s a -> Int -> ST s Bool
deleteST im = IS.delete (setIM im)

{-# INLINEABLE deleteST_ #-}
deleteST_ :: IntMap s a -> Int -> ST s ()
deleteST_ im = IS.delete_ (setIM im)

{-# INLINEABLE deleteMinST #-}
deleteMinST :: (HasCallStack, VU.Unbox a) => IntMap s a -> ST s (Maybe (Int, a))
deleteMinST is = do
  lookupMinST is
    >>= mapM
      ( \(!key, !val) -> do
          deleteST_ is key
          pure (key, val)
      )

{-# INLINEABLE deleteMaxST #-}
deleteMaxST :: (HasCallStack, VU.Unbox a) => IntMap s a -> ST s (Maybe (Int, a))
deleteMaxST is = do
  lookupMaxST is
    >>= mapM
      ( \(!k, !v) -> do
          deleteST_ is k
          pure (k, v)
      )

{-# INLINEABLE keysST #-}
keysST :: IntMap s a -> ST s (VU.Vector Int)
keysST = IS.keys . setIM

{-# INLINEABLE elemsST #-}
elemsST :: (VU.Unbox a) => IntMap s a -> ST s (VU.Vector a)
elemsST im@IntMap {..} = do
  n <- IS.size setIM
  VU.unfoldrExactNM
    n
    ( \i -> do
        (!i', !x') <- fromJust <$> lookupGEST im (i + 1)
        pure (x', i')
    )
    (-1)

{-# INLINEABLE assocsST #-}
assocsST :: (VU.Unbox a) => IntMap s a -> ST s (VU.Vector (Int, a))
assocsST im@IntMap {..} = do
  n <- IS.size setIM
  VU.unfoldrExactNM
    n
    ( \i -> do
        (!i', !x') <- fromJust <$> lookupGEST im (i + 1)
        pure ((i', x'), i')
    )
    (-1)
