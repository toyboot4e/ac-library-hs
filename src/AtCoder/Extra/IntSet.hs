{-# LANGUAGE RecordWildCards #-}

-- original implementation:
-- <https://github.com/maspypy/library/blob/main/ds/fastset.hpp>

-- | A dense, fast `Int` set implemented as a 64-ary tree that covers an interval \([0, n)\).
--
-- ==== __Example__
-- Create an `IntSet` with capacity \(10\):
--
-- >>> import AtCoder.Extra.IntSet qualified as IS
-- >>> is <- IS.new @_ 10
--
-- `insert`, `delete` and other functions are available:
--
-- >>> IS.insert is 0
-- >>> IS.insert is 9
-- >>> IS.member is 9
-- True
--
-- >>> IS.delete is 0
-- True
--
-- >>> IS.size is
-- 1
--
-- >>> IS.member is 1
-- False
--
-- >>> IS.lookupGT is 5
-- Just 9
--
-- @since 1.1.0.0
module AtCoder.Extra.IntSet
  ( -- * IntSet
    IntSet,

    -- * Constructors
    new,
    build,

    -- * Metadata
    capacity,
    size,
    null,

    -- * Lookups
    member,
    notMember,

    -- ** Compartive lookups
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

    -- ** Deletions
    delete,
    delete_,
    deleteMin,
    deleteMax,

    -- * Conversions
    keys,
  )
where

import AtCoder.Internal.Assert qualified as ACIA
import Control.Monad (unless, void)
import Control.Monad.Primitive (PrimMonad, PrimState, stToPrim)
import Control.Monad.ST (ST)
import Data.Bifunctor (bimap)
import Data.Bits
  ( Bits (clearBit, setBit, testBit),
    FiniteBits (countLeadingZeros, countTrailingZeros),
    (.<<.),
    (.>>.),
  )
import Data.Maybe (fromJust)
import Data.Vector qualified as V
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Stack (HasCallStack)
import Prelude hiding (null)

-- | \(O(1)\) Retrieves the most significant bit.
--
-- >>> msbOf 0
-- -1
--
-- >>> msbOf maxBound
-- 62
--
-- >>> msbOf $ 4 + 2 + 1
-- 2
{-# INLINE msbOf #-}
msbOf :: Int -> Int
msbOf !x = 63 - countLeadingZeros x

-- | \(O(1)\) Retrieves the least significant bit.
--
-- >>> lsbOf 0
-- -1
--
-- >>> lsbOf maxBound
-- 0
--
-- >>> lsbOf $ 4 + 2 + 1
-- 0
{-# INLINE lsbOf #-}
lsbOf :: Int -> Int
lsbOf 0 = -1
lsbOf x = countTrailingZeros x

{-# INLINE wordSize #-}
wordSize :: Int
wordSize = 64

-- | A dense, fast `Int` set implemented as a 64-ary tree that covers an interval \([0, n)\).
--
-- @since 1.1.0.0
data IntSet s = IntSet
  { -- | Maximum number of elements.
    capacityIS :: {-# UNPACK #-} !Int,
    -- | The number of elements.
    sizeIS :: !(VUM.MVector s Int),
    -- | Segments.
    vecIS :: !(V.Vector (VUM.MVector s Int))
  }

-- | \(O(n)\) Creates an `IntSet` for the interval \([0, n)\).
--
-- @since 1.1.0.0
{-# INLINE new #-}
new :: (PrimMonad m) => Int -> m (IntSet (PrimState m))
new capacityIS = stToPrim $ newST capacityIS

-- | \(O(n + m \log n)\) Creates an `IntSet` for the interval \([0, n)\) with initial values.
--
-- @since 1.1.0.0
{-# INLINE build #-}
build :: (PrimMonad m) => Int -> VU.Vector Int -> m (IntSet (PrimState m))
build n vs = stToPrim $ buildST n vs

-- | \(O(1)\) Returns the capacity \(n\), where the interval \([0, n)\) is covered by the set.
--
-- @since 1.1.0.0
{-# INLINE capacity #-}
capacity :: IntSet s -> Int
capacity = capacityIS

-- | \(O(1)\) Returns the number of elements in the set.
--
-- @since 1.1.0.0
{-# INLINE size #-}
size :: (PrimMonad m) => IntSet (PrimState m) -> m Int
size = (`VUM.unsafeRead` 0) . sizeIS

-- | \(O(1)\) Returns whether the set is empty.
--
-- @since 1.1.0.0
{-# INLINE null #-}
null :: (PrimMonad m) => IntSet (PrimState m) -> m Bool
null = ((== 0) <$>) . size

-- | \(O(\log n)\) Tests whether \(k\) is in the set.
--
-- @since 1.1.0.0
{-# INLINE member #-}
member :: (PrimMonad m) => IntSet (PrimState m) -> Int -> m Bool
member is k = stToPrim $ memberST is k

-- | \(O(\log n)\) Tests whether \(k\) is not in the set.
--
-- @since 1.1.0.0
{-# INLINE notMember #-}
notMember :: (PrimMonad m) => IntSet (PrimState m) -> Int -> m Bool
notMember dis k = stToPrim $ not <$> memberST dis k

-- | \(O(\log n)\) Looks up the smallest key \(k\) such that \(k \ge k_0\).
--
-- @since 1.1.0.0
{-# INLINE lookupGE #-}
lookupGE :: (PrimMonad m) => IntSet (PrimState m) -> Int -> m (Maybe Int)
lookupGE is i0 = stToPrim $ lookupGEST is i0

-- | \(O(\log n)\) Looks up the smallest key \(k\) such that \(k \gt k_0\).
--
-- @since 1.1.0.0
{-# INLINE lookupGT #-}
lookupGT :: (PrimMonad m) => IntSet (PrimState m) -> Int -> m (Maybe Int)
lookupGT is k = stToPrim $ lookupGTST is k

-- | \(O(\log n)\) Looks up the largest key \(k\) such that \(k \le k_0\).
--
-- @since 1.1.0.0
{-# INLINE lookupLE #-}
lookupLE :: (PrimMonad m) => IntSet (PrimState m) -> Int -> m (Maybe Int)
lookupLE is i0 = stToPrim $ lookupLEST is i0

-- | \(O(\log n)\) Looks up the largest key \(k\) such that \(k \lt k_0\).
--
-- @since 1.1.0.0
{-# INLINE lookupLT #-}
lookupLT :: (PrimMonad m) => IntSet (PrimState m) -> Int -> m (Maybe Int)
lookupLT is k = stToPrim $ lookupLTST is k

-- | \(O(\log n)\) Looks up the minimum key.
--
-- @since 1.1.0.0
{-# INLINE lookupMin #-}
lookupMin :: (PrimMonad m) => IntSet (PrimState m) -> m (Maybe Int)
lookupMin is = stToPrim $ lookupMinST is

-- | \(O(\log n)\) Looks up the maximum key.
--
-- @since 1.1.0.0
{-# INLINE lookupMax #-}
lookupMax :: (PrimMonad m) => IntSet (PrimState m) -> m (Maybe Int)
lookupMax is = stToPrim $ lookupMaxST is

-- | \(O(\log n)\) Inserts a key \(k\) into the set. If an entry with the same key already exists,
-- it is overwritten.
--
-- @since 1.1.0.0
{-# INLINE insert #-}
insert :: (HasCallStack, PrimMonad m) => IntSet (PrimState m) -> Int -> m ()
insert is k = stToPrim $ insertST is k

-- | \(O(\log n)\) Deletes a key \(k\) from the set. Does nothing if no such key exists. Returns
-- whether the key existed.
--
-- @since 1.1.0.0
{-# INLINE delete #-}
delete :: (PrimMonad m) => IntSet (PrimState m) -> Int -> m Bool
delete is k = stToPrim $ deleteST is k

-- | \(O(\log n)\) Deletes a key \(k\) from the set. Does nothing if no such key exists.
--
-- @since 1.1.0.0
{-# INLINE delete_ #-}
delete_ :: (PrimMonad m) => IntSet (PrimState m) -> Int -> m ()
delete_ is k = stToPrim $ deleteST_ is k

-- | \(O(\log n)\) Deletes the minimum key from the set. Returns `Nothing` if the set is empty.
--
-- @since 1.1.0.0
{-# INLINE deleteMin #-}
deleteMin :: (PrimMonad m) => IntSet (PrimState m) -> m (Maybe Int)
deleteMin is = stToPrim $ deleteMinST is

-- | \(O(\log n)\) Deletes the maximum key from the set. Returns `Nothing` if the set is empty.
--
-- @since 1.1.0.0
{-# INLINE deleteMax #-}
deleteMax :: (PrimMonad m) => IntSet (PrimState m) -> m (Maybe Int)
deleteMax is = stToPrim $ deleteMaxST is

-- | \(O(n \log n)\) Enumerates the keys in the map.
--
-- @since 1.1.0.0
{-# INLINE keys #-}
keys :: (PrimMonad m) => IntSet (PrimState m) -> m (VU.Vector Int)
keys is = stToPrim $ keysST is

-- -------------------------------------------------------------------------------------------------
-- Internal
-- -------------------------------------------------------------------------------------------------

{-# INLINEABLE newST #-}
newST :: Int -> ST s (IntSet s)
newST capacityIS = do
  vecIS <-
    V.unfoldrExactNM
      (max 1 logSize)
      ( \len -> do
          let !len' = (len + wordSize - 1) `div` wordSize
          (,len') <$> VUM.replicate len' 0
      )
      capacityIS
  sizeIS <- VUM.replicate 1 (0 :: Int)
  pure IntSet {..}
  where
    (!_, !logSize) =
      until
        ((<= 1) . fst)
        (bimap ((`div` wordSize) . (+ (wordSize - 1))) (+ 1))
        (capacityIS, 0)

{-# INLINEABLE buildST #-}
buildST :: Int -> VU.Vector Int -> ST s (IntSet s)
buildST n vs = do
  set <- newST n
  VU.forM_ vs (insertST set)
  pure set

{-# INLINEABLE memberST #-}
memberST :: IntSet s -> Int -> ST s Bool
memberST IntSet {..} k
  | ACIA.testIndex k capacityIS = do
      let (!q, !r) = k `divMod` wordSize
      (`testBit` r) <$> VGM.unsafeRead (VG.unsafeHead vecIS) q
  | otherwise = pure False

{-# INLINEABLE lookupGEST #-}
lookupGEST :: IntSet s -> Int -> ST s (Maybe Int)
lookupGEST IntSet {..} i0
  | i0 >= capacityIS = pure Nothing
  | otherwise = inner 0 $ max 0 i0 -- REMARK: it's very important to keep @i@ non-negative.
  where
    inner h i
      | h >= V.length vecIS = pure Nothing
      -- ?
      | q == VUM.length (VG.unsafeIndex vecIS h) = pure Nothing
      | otherwise = do
          d <- (.>>. r) <$> VGM.unsafeRead (VG.unsafeIndex vecIS h) q
          if d == 0
            then inner (h + 1) (q + 1)
            else
              Just
                <$> V.foldM'
                  ( \ !acc vec -> do
                      !dx <- lsbOf <$> VGM.unsafeRead vec acc
                      pure $ acc * wordSize + dx
                  )
                  (i + lsbOf d)
                  (V.unsafeBackpermute vecIS (V.enumFromStepN (h - 1) (-1) h))
      where
        (!q, !r) = i `divMod` wordSize

{-# INLINEABLE lookupGTST #-}
lookupGTST :: IntSet s -> Int -> ST s (Maybe Int)
lookupGTST is k = lookupGEST is (k + 1)

{-# INLINEABLE lookupLEST #-}
lookupLEST :: IntSet s -> Int -> ST s (Maybe Int)
lookupLEST IntSet {..} i0
  | i0 <= -1 = pure Nothing
  | otherwise = inner 0 $ min (capacityIS - 1) i0
  where
    inner h i
      | h >= V.length vecIS = pure Nothing
      | i == -1 = pure Nothing
      | otherwise = do
          d <- (.<<. (63 - r)) <$> VGM.unsafeRead (VG.unsafeIndex vecIS h) q
          if d == 0
            then inner (h + 1) (q - 1)
            else do
              Just
                <$> V.foldM'
                  ( \ !acc vec -> do
                      !dx <- msbOf <$> VGM.unsafeRead vec acc
                      pure $ acc * wordSize + dx
                  )
                  (i - countLeadingZeros d)
                  (V.unsafeBackpermute vecIS (V.enumFromStepN (h - 1) (-1) h))
      where
        (!q, !r) = i `divMod` wordSize

{-# INLINEABLE lookupLTST #-}
lookupLTST :: IntSet s -> Int -> ST s (Maybe Int)
lookupLTST is k = lookupLEST is (k - 1)

{-# INLINEABLE lookupMinST #-}
lookupMinST :: IntSet s -> ST s (Maybe Int)
lookupMinST is = lookupGEST is 0

{-# INLINEABLE lookupMaxST #-}
lookupMaxST :: IntSet s -> ST s (Maybe Int)
lookupMaxST is = lookupLEST is (capacityIS is - 1)

{-# INLINEABLE insertST #-}
insertST :: (HasCallStack) => IntSet s -> Int -> ST s ()
insertST is@IntSet {..} k = do
  b <- memberST is k
  unless b $ do
    VUM.unsafeModify sizeIS (+ 1) 0
    V.foldM'_
      ( \i vec -> do
          let (!q, !r) = i `divMod` wordSize
          VGM.unsafeModify vec (`setBit` r) q
          pure q
      )
      k
      vecIS
  where
    !_ = ACIA.checkIndex "AtCoder.Extra.IntSet.insert" k capacityIS

{-# INLINEABLE deleteST #-}
deleteST :: IntSet s -> Int -> ST s Bool
deleteST is@IntSet {..} k = do
  b_ <- memberST is k
  if b_
    then do
      VUM.unsafeModify sizeIS (subtract 1) 0
      V.foldM'_
        ( \(!b, !i) vec -> do
            let (!q, !r) = i `divMod` wordSize
            -- TODO: early return is possible
            unless b $ do
              VGM.unsafeModify vec (`clearBit` r) q
            -- `b` remembers if any other bit was on
            b' <- (/= 0) <$> VGM.unsafeRead vec q
            pure (b', q)
        )
        (False, k)
        vecIS
      pure True
    else pure False

{-# INLINEABLE deleteST_ #-}
deleteST_ :: IntSet s -> Int -> ST s ()
deleteST_ is k = void $ deleteST is k

{-# INLINEABLE deleteMinST #-}
deleteMinST :: IntSet s -> ST s (Maybe Int)
deleteMinST is = do
  lookupMinST is
    >>= mapM
      ( \key -> do
          deleteST_ is key
          pure key
      )

{-# INLINEABLE deleteMaxST #-}
deleteMaxST :: IntSet s -> ST s (Maybe Int)
deleteMaxST is = do
  lookupMaxST is
    >>= mapM
      ( \key -> do
          deleteST_ is key
          pure key
      )

{-# INLINEABLE keysST #-}
keysST :: IntSet s -> ST s (VU.Vector Int)
keysST is@IntSet {sizeIS} = do
  n <- VGM.unsafeRead sizeIS 0
  VU.unfoldrExactNM
    n
    ( \i -> do
        i' <- fromJust <$> lookupGTST is i
        pure (i', i')
    )
    (-1)
