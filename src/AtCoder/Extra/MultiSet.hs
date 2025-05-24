{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- | A fast, mutable multiset for `Int` keys backed by a @HashMap@. Most operations are performed
-- in \(O(1)\) on average.
--
-- ==== Capacity limitation
-- Access to each key creates a new entry. Note that entries cannot be invalidated due to the
-- internal implementation (called /open addressing/). If the hash map is full,
-- __access to a new key causes infinite loop__.
--
-- ==== Invariant
-- The count for each key must be non-negative. An exception is thrown if this invariant is
-- violated on `add` or `sub`.
--
-- ==== __Example__
-- Create a `MultiSet` with capacity \(4\):
--
-- >>> import AtCoder.Extra.MultiSet qualified as MS
-- >>> ms <- MS.new 4
--
-- `inc` and `dec` are the primary API:
--
-- >>> MS.inc ms 10
-- >>> MS.inc ms 10
-- >>> MS.lookup ms 10
-- Just 2
--
-- >>> MS.dec ms 10
-- >>> MS.lookup ms 10
-- Just 1
--
-- Entries with zero count are considered to be non-existing:
--
-- >>> MS.dec ms 10
-- >>> MS.member ms 10
-- False
--
-- >>> MS.lookup ms 10
-- Nothing
--
-- >>> MS.size ms
-- 0
--
-- Creating a negative count results in an exception:
--
-- >>> MS.inc ms 11
-- >>> MS.sub ms 11 2
-- *** Exception: AtCoder.Extra.Multiset.subST: the count of `11` is becoming a negative value: `-1`
-- ...
--
-- Decrementing a non-existing key does nothing and does not throw an exception:
--
-- >>> MS.dec ms 12
--
-- Misc:
--
-- >>> MS.insert ms 12 112
-- >>> MS.assocs ms
-- [(11,1),(12,112)]
--
-- @since 1.1.0.0
module AtCoder.Extra.MultiSet
  ( -- * MultiSet
    MultiSet,

    -- * Construtors
    new,

    -- * Metadata
    capacity,
    size,

    -- * Lookups
    lookup,
    member,
    notMember,

    -- * Modifications
    inc,
    dec,
    add,
    sub,
    insert,
    delete,

    -- * Conversions

    -- ** Safe conversions
    keys,
    elems,
    assocs,

    -- ** Unsafe conversions
    unsafeKeys,
    unsafeElems,
    unsafeAssocs,
  )
where

import AtCoder.Extra.HashMap qualified as HM
import Control.Monad (when)
import Control.Monad.Primitive (PrimMonad, PrimState, stToPrim)
import Control.Monad.ST (ST)
import Data.Functor ((<&>))
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Stack (HasCallStack)
import Prelude hiding (lookup)

-- | A fast, mutable multiset for `Int` keys backed by a @HashMap@.
--
-- @since 1.1.0.0
data MultiSet s = MultiSet
  { mapMS :: !(HM.HashMap s Int),
    cntMS :: !(VUM.MVector s Int)
  }

-- | \(O(n)\) Creates a `MultiSet` with capacity \(n\).
--
-- @since 1.1.0.0
{-# INLINE new #-}
new :: (PrimMonad m) => Int -> m (MultiSet (PrimState m))
new n = stToPrim $ newST n

-- | \(O(1)\) Returns the maximum number of distinct keys that can be inserted into the internal
-- hash map.
--
-- @since 1.1.0.0
{-# INLINE capacity #-}
capacity :: MultiSet s -> Int
capacity = HM.capacity . mapMS

-- | \(O(1)\) Returns the number of distinct keys with positive counts.
--
-- @since 1.1.0.0
{-# INLINE size #-}
size :: (PrimMonad m) => MultiSet (PrimState m) -> m Int
size MultiSet {..} = do
  VGM.unsafeRead cntMS 0

-- | \(O(1)\) Looks up the count for a key.
--
-- @since 1.1.0.0
{-# INLINE lookup #-}
lookup :: (PrimMonad m) => MultiSet (PrimState m) -> Int -> m (Maybe Int)
lookup ms k = stToPrim $ lookupST ms k

-- | \(O(1)\) Tests whether \(k\) is in the set.
--
-- @since 1.1.0.0
{-# INLINE member #-}
member :: (PrimMonad m) => MultiSet (PrimState m) -> Int -> m Bool
member ms k = stToPrim $ memberST ms k

-- | \(O(1)\) Tests whether \(k\) is not in the set.
--
-- @since 1.1.0.0
{-# INLINE notMember #-}
notMember :: (PrimMonad m) => MultiSet (PrimState m) -> Int -> m Bool
notMember ms k = stToPrim $ not <$> memberST ms k

-- | \(O(1)\) Increments the count of a key.
--
-- @since 1.1.0.0
{-# INLINE inc #-}
inc :: (HasCallStack, PrimMonad m) => MultiSet (PrimState m) -> Int -> m ()
inc ms k = add ms k 1

-- | \(O(1)\) Decrements the count of a key.
--
-- @since 1.1.0.0
{-# INLINE dec #-}
dec :: (HasCallStack, PrimMonad m) => MultiSet (PrimState m) -> Int -> m ()
dec ms k = sub ms k 1

-- | \(O(1)\) Increments the count of a key \(k\) by \(c\). If the key does not exist in the set,
-- the \((k, c)\) pair is inserted. If \(v\) is negative, it falls back to `sub`.
--
-- @since 1.1.0.0
{-# INLINE add #-}
add :: (HasCallStack, PrimMonad m) => MultiSet (PrimState m) -> Int -> Int -> m ()
add ms k v = stToPrim $ addST ms k v

-- | \(O(1)\) Decrements the count of a key \(k\) by \(c\). If \(c\) is negative, it falls back to
-- `add`.
--
-- @since 1.1.0.0
{-# INLINE sub #-}
sub :: (HasCallStack, PrimMonad m) => MultiSet (PrimState m) -> Int -> Int -> m ()
sub ms k v = stToPrim $ subST ms k v

-- | \(O(1)\) Inserts a key-count pair into the set. `MultiSet` is actually a count map.
--
-- @since 1.1.0.0
{-# INLINE insert #-}
insert :: (HasCallStack, PrimMonad m) => MultiSet (PrimState m) -> Int -> Int -> m ()
insert ms k v = stToPrim $ insertST ms k v

-- | \(O(1)\) Deletes a key. Note that it does not undo its insertion and does not increase the
-- number of distinct keys that can be inserted into the internal hash map.
--
-- @since 1.1.0.0
{-# INLINE delete #-}
delete :: (HasCallStack, PrimMonad m) => MultiSet (PrimState m) -> Int -> m ()
delete ms k = stToPrim $ deleteST ms k

-- | \(O(n)\) Enumerates the keys in the set.
--
-- @since 1.1.0.0
{-# INLINE keys #-}
keys :: (PrimMonad m) => MultiSet (PrimState m) -> m (VU.Vector Int)
keys ms = VU.force <$> unsafeKeys ms

-- | \(O(n)\) Enumerates the counts in the set.
--
-- @since 1.1.0.0
{-# INLINE elems #-}
elems :: (PrimMonad m) => MultiSet (PrimState m) -> m (VU.Vector Int)
elems ms = VU.force <$> unsafeElems ms

-- | \(O(n)\) Enumerates the key-count pairs in the set.
--
-- @since 1.1.0.0
{-# INLINE assocs #-}
assocs :: (PrimMonad m) => MultiSet (PrimState m) -> m (VU.Vector (Int, Int))
assocs ms = VU.force <$> unsafeAssocs ms

-- | \(O(n)\) Enumerates the keys in the set.
--
-- @since 1.1.0.0
{-# INLINE unsafeKeys #-}
unsafeKeys :: (PrimMonad m) => MultiSet (PrimState m) -> m (VU.Vector Int)
unsafeKeys = (VU.mapMaybe (\(!k, !n) -> if n > 0 then Just k else Nothing) <$>) . HM.unsafeAssocs . mapMS

-- | \(O(n)\) Enumerates the counts in the set.
--
-- @since 1.1.0.0
{-# INLINE unsafeElems #-}
unsafeElems :: (PrimMonad m) => MultiSet (PrimState m) -> m (VU.Vector Int)
unsafeElems = (VU.filter (> 0) <$>) . HM.unsafeElems . mapMS

-- | \(O(n)\) Enumerates the key-count pairs in the set.
--
-- @since 1.1.0.0
{-# INLINE unsafeAssocs #-}
unsafeAssocs :: (PrimMonad m) => MultiSet (PrimState m) -> m (VU.Vector (Int, Int))
unsafeAssocs = (VU.filter (\(!_, !n) -> n > 0) <$>) . HM.unsafeAssocs . mapMS

-- -------------------------------------------------------------------------------
-- Internal
-- -------------------------------------------------------------------------------

{-# INLINEABLE newST #-}
newST :: Int -> ST s (MultiSet s)
newST n = do
  mapMS <- HM.new n
  cntMS <- VUM.replicate 1 0
  pure $ MultiSet {..}

{-# INLINEABLE lookupST #-}
lookupST :: MultiSet s -> Int -> ST s (Maybe Int)
lookupST MultiSet {..} k = do
  HM.lookup mapMS k <&> \case
    Just i | i > 0 -> Just i
    _ -> Nothing

{-# INLINEABLE memberST #-}
memberST :: MultiSet s -> Int -> ST s Bool
memberST MultiSet {..} k = do
  HM.lookup mapMS k <&> \case
    Just i -> i > 0
    _ -> False

{-# INLINEABLE addST #-}
addST :: (HasCallStack) => MultiSet s -> Int -> Int -> ST s ()
addST ms@MultiSet {..} k v = case compare v 0 of
  LT -> subST ms k (-v)
  EQ -> pure ()
  GT -> do
    HM.lookup mapMS k >>= \case
      Just n -> do
        HM.insert mapMS k $ n + v
        when (n <= 0) $ do
          VGM.unsafeModify cntMS (+ 1) 0
      Nothing -> do
        HM.insert mapMS k v
        VGM.unsafeModify cntMS (+ 1) 0

{-# INLINEABLE subST #-}
subST :: (HasCallStack) => MultiSet s -> Int -> Int -> ST s ()
subST ms@MultiSet {..} k v = case compare v 0 of
  LT -> addST ms k (-v)
  EQ -> pure ()
  GT -> do
    HM.lookup mapMS k >>= \case
      Just 0 -> pure () -- ignored
      Just n -> case compare n v of
        GT -> do
          HM.insert mapMS k (n - v)
        EQ -> do
          HM.insert mapMS k 0
          VGM.unsafeModify cntMS (subtract 1) 0
        LT -> error $ "AtCoder.Extra.Multiset.subST: the count of `" ++ show k ++ "` is becoming a negative value: `" ++ show (n - v) ++ "`"
      _ -> pure ()

{-# INLINEABLE insertST #-}
insertST :: (HasCallStack) => MultiSet s -> Int -> Int -> ST s ()
insertST MultiSet {..} k v
  | v <= 0 = error $ "AtCoder.Extra.Multiset.insertST: new count must be positive`" ++ show k ++ "`: `" ++ show v ++ "`"
  | otherwise = do
      HM.lookup mapMS k >>= \case
        Just n | n > 0 -> do
          HM.insert mapMS k v
        _ -> do
          HM.insert mapMS k v
          VGM.unsafeModify cntMS (+ 1) 0

{-# INLINEABLE deleteST #-}
deleteST :: (HasCallStack) => MultiSet s -> Int -> ST s ()
deleteST MultiSet {..} k = do
  HM.lookup mapMS k >>= \case
    Just i | i > 0 -> do
      HM.insert mapMS k 0
      VGM.unsafeModify cntMS (subtract 1) 0
    _ -> pure ()
