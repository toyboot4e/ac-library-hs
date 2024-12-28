{-# LANGUAGE RecordWildCards #-}

-- original implementation:
-- <https://github.com/maspypy/library/blob/main/ds/fastset.hpp>

-- | A dense, fast `Int` set implemented as a 64-ary tree that covers the interval \([0, n)\).
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

    -- * Lookups
    member,
    notMember,

    -- ** Compartive
    lookupGE,
    lookupGT,
    lookupLE,
    lookupLT,

    -- ** Max/Min
    lookupMin,
    lookupMax,

    -- * Modifications

    -- ** Inserting
    insert,

    -- ** Deleting
    delete,
    delete_,
    deleteMin,
    deleteMax,

    -- * Conversion
    keys,
  )
where

import AtCoder.Internal.Assert qualified as ACIA
import Control.Monad (unless, void)
import Control.Monad.Primitive (PrimMonad, PrimState)
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

-- | A dense, fast `Int` set implemented as a 64-ary tree that covers the interval \([0, n)\).
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
new capacityIS = do
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

-- | \(O(n \log n)\) Creates an `IntSet` for the interval \([0, n)\) with initial values.
--
-- @since 1.1.0.0
{-# INLINE build #-}
build :: (PrimMonad m) => Int -> VU.Vector Int -> m (IntSet (PrimState m))
build n vs = do
  set <- new n
  VU.forM_ vs (insert set)
  pure set

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

-- | \(O(\log n)\) Tests whether \(k\) is in the map.
--
-- @since 1.1.0.0
{-# INLINE member #-}
member :: (PrimMonad m) => IntSet (PrimState m) -> Int -> m Bool
member IntSet {..} k
  | ACIA.testIndex k capacityIS = do
      let (!q, !r) = k `divMod` wordSize
      (`testBit` r) <$> VGM.unsafeRead (VG.unsafeHead vecIS) q
  | otherwise = pure False

-- | \(O(\log n)\) Tests whether \(k\) is not in the set.
--
-- @since 1.1.0.0
{-# INLINE notMember #-}
notMember :: (PrimMonad m) => IntSet (PrimState m) -> Int -> m Bool
notMember dis k = not <$> member dis k

-- | \(O(\log n)\) Deletes \(k\) from the set. Does nothing if no such key exists.
--
-- @since 1.1.0.0
{-# INLINE delete_ #-}
delete_ :: (PrimMonad m) => IntSet (PrimState m) -> Int -> m ()
delete_ is k = void $ delete is k

-- | \(O(\log n)\) Looks up the smallest \(k\) such that \(k \ge k_0\) in the set.
--
-- @since 1.1.0.0
{-# INLINE lookupGE #-}
lookupGE :: (PrimMonad m) => IntSet (PrimState m) -> Int -> m (Maybe Int)
lookupGE IntSet {..} i0
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

-- | \(O(\log n)\) Looks up the smallest \(k\) such that \(k \gt k_0\) in the set.
--
-- @since 1.1.0.0
{-# INLINE lookupGT #-}
lookupGT :: (PrimMonad m) => IntSet (PrimState m) -> Int -> m (Maybe Int)
lookupGT is k = lookupGE is (k + 1)

-- | \(O(\log n)\) Looks up the largest \(k\) such that \(k \le k_0\) in the set.
--
-- @since 1.1.0.0
{-# INLINE lookupLE #-}
lookupLE :: (PrimMonad m) => IntSet (PrimState m) -> Int -> m (Maybe Int)
lookupLE IntSet {..} i0
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

-- | \(O(\log n)\) Looks up the largest \(k\) such that \(k \lt k_0\) in the set.
--
-- @since 1.1.0.0
{-# INLINE lookupLT #-}
lookupLT :: (PrimMonad m) => IntSet (PrimState m) -> Int -> m (Maybe Int)
lookupLT is k = lookupLE is (k - 1)

-- | \(O(\log n)\) Looks up minimum value in the set.
--
-- @since 1.1.0.0
{-# INLINE lookupMin #-}
lookupMin :: (PrimMonad m) => IntSet (PrimState m) -> m (Maybe Int)
lookupMin is = lookupGE is 0

-- | \(O(\log n)\) Looks up maximum value in the set.
--
-- @since 1.1.0.0
{-# INLINE lookupMax #-}
lookupMax :: (PrimMonad m) => IntSet (PrimState m) -> m (Maybe Int)
lookupMax is = lookupLE is (capacityIS is - 1)

-- | \(O(\log n)\) Inserts \(k\) into the set. If an entry with the same key already exists, it is
-- overwritten.
--
-- @since 1.1.0.0
{-# INLINE insert #-}
insert :: (HasCallStack, PrimMonad m) => IntSet (PrimState m) -> Int -> m ()
insert is@IntSet {..} k = do
  b <- member is k
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

-- | \(O(\log n)\) Deletes @k@ from the set. Does nothing if no such key exists. Returns whether the
-- key existed.
--
-- @since 1.1.0.0
{-# INLINE delete #-}
delete :: (PrimMonad m) => IntSet (PrimState m) -> Int -> m Bool
delete is@IntSet {..} k = do
  b_ <- member is k
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

-- | \(O(\log n)\) Deletes the minimum value in the set.
--
-- @since 1.1.0.0
{-# INLINE deleteMin #-}
deleteMin :: (PrimMonad m) => IntSet (PrimState m) -> m (Maybe Int)
deleteMin is = do
  lookupMin is
    >>= mapM
      ( \key -> do
          delete_ is key
          pure key
      )

-- | \(O(\log n)\) Deletes the maximum value in the set.
--
-- @since 1.1.0.0
{-# INLINE deleteMax #-}
deleteMax :: (PrimMonad m) => IntSet (PrimState m) -> m (Maybe Int)
deleteMax is = do
  lookupMax is
    >>= mapM
      ( \key -> do
          delete_ is key
          pure key
      )

-- | \(O(n \log n)\) Enumerates the keys in the map.
--
-- @since 1.1.0.0
{-# INLINE keys #-}
keys :: (PrimMonad m) => IntSet (PrimState m) -> m (VU.Vector Int)
keys is@IntSet {sizeIS} = do
  n <- VGM.unsafeRead sizeIS 0
  VU.unfoldrExactNM
    n
    ( \i -> do
        i' <- fromJust <$> lookupGT is i
        pure (i', i')
    )
    (-1)
