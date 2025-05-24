{-# LANGUAGE RecordWildCards #-}

-- | A compact bit vector, a collection of bits that can process @rank@ in \(O(1)\) and @select@ in
-- \(O(\log n)\).
--
-- @since 1.1.0.0
module AtCoder.Extra.WaveletMatrix.BitVector
  ( -- * Bit vector
    BitVector (..),

    -- * Constructor
    build,

    -- * (Internal) Word-based cumulative sum
    wordSize,
    csumInPlace,

    -- * Rank
    rank0,
    rank1,

    -- * Select
    select0,
    select1,
    selectKthIn0,
    selectKthIn1,
  )
where

import AtCoder.Extra.Bisect (maxRight)
import Control.Monad.Primitive (PrimMonad (PrimState))
import Data.Bit (Bit (..))
import Data.Bits (popCount)
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM

-- | A compact bit vector.
--
-- @since 1.1.0.0
data BitVector = BitVector
  { -- | Packed bits.
    --
    -- @since 1.1.0.0
    bitsBv :: !(VU.Vector Bit),
    -- | Cumulative sum of bits by 64 words.
    --
    -- @since 1.1.0.0
    csumBv :: !(VU.Vector Int)
    -- we could use Word32 for csumBv, as 2^32 is large enough
  }
  deriving (Eq, Show)

-- | \(O(n)\) Creates a `BitVector`.
--
-- @since 1.1.0.0
{-# INLINE build #-}
build :: VU.Vector Bit -> BitVector
build bitsBv =
  let csumBv = VU.create $ do
        vec <- VUM.replicate ((VU.length bitsBv + 63) `div` 64 + 1) 0
        _ <- csumInPlace vec bitsBv
        pure vec
   in BitVector {..}

-- | The block size \(64\) for the internal cumultaive sum in the bit vector.
--
-- @since 1.1.0.0
{-# INLINE wordSize #-}
wordSize :: Int
wordSize = 64

-- | \(O(n)\) Calculates the cumulative sum in-place for the bit vector and returns the sum.
--
-- @since 1.1.0.0
{-# INLINE csumInPlace #-}
csumInPlace ::
  (PrimMonad m) =>
  -- | Cumulative sum of length \(\lceil |\mathrm{bits}| / 64 \rceil\).
  VUM.MVector (PrimState m) Int ->
  -- | Bits
  VU.Vector Bit ->
  -- | Sum of the bits
  m Int
csumInPlace csum bits = do
  VGM.unsafeWrite csum 0 (0 :: Int)

  -- Calcuate popCount by word. TODO: use `castToWords` for most elements
  VU.ifoldM'
    ( \ !acc i wordSum -> do
        let !acc' = acc + wordSum
        VGM.unsafeWrite csum (i + 1) acc'
        pure acc'
    )
    (0 :: Int)
    $ VU.unfoldrExactN
      (VGM.length csum - 1)
      (\bits' -> (popCount (VU.take wordSize bits'), VU.drop wordSize bits'))
      bits

-- | \(O(1)\) Counts the number of \(0\) bits in the interval \([0, i)\).
--
-- @since 1.1.0.0
{-# INLINE rank0 #-}
rank0 :: BitVector -> Int -> Int
rank0 bv i = i - rank1 bv i

-- | \(O(1)\) Counts the number of \(1\) bits in an interval \([0, i)\).
--
-- @since 1.1.0.0
{-# INLINE rank1 #-}
rank1 :: BitVector -> Int -> Int
rank1 BitVector {..} i = fromCSum + fromRest
  where
    -- TODO: check bounds for i?
    (!nWords, !nRest) = i `divMod` wordSize
    fromCSum = VG.unsafeIndex csumBv nWords
    fromRest = popCount . VU.take nRest . VU.drop (nWords * wordSize) $ bitsBv

-- | \(O(\log n)\) Returns the index of \(k\)-th \(0\) (0-based), or `Nothing` if no such bit exists.
--
-- @since 1.1.0.0
{-# INLINE select0 #-}
select0 :: BitVector -> Int -> Maybe Int
select0 bv = selectKthIn0 bv 0 (VG.length (bitsBv bv))

-- | \(O(\log n)\) Returns the index of \(k\)-th \(1\) (0-based), or `Nothing` if no such bit exists.
--
-- @since 1.1.0.0
{-# INLINE select1 #-}
select1 :: BitVector -> Int -> Maybe Int
select1 bv = selectKthIn1 bv 0 (VG.length (bitsBv bv))

-- | \(O(\log n)\) Given an interval \([l, r)\), it returns the index of the first occurrence
-- (0-based) of \(0\) in the sequence, or `Nothing` if no such occurrence exists.
--
-- @since 1.1.0.0
{-# INLINE selectKthIn0 #-}
selectKthIn0 ::
  -- | A bit vector
  BitVector ->
  -- | \(l\)
  Int ->
  -- | \(r\)
  Int ->
  -- | \(k\)
  Int ->
  -- | The index of \(k\)-th \(0\) in \([l, r)\)
  Maybe Int
selectKthIn0 bv l r k
  | k < 0 || nZeros <= k = Nothing
  -- note that `rank0` takes exclusive index
  | otherwise = Just . maxRight l r $ \i -> rank0 bv (i + 1) - rankL0 < k + 1
  where
    nZeros = rank0 bv r - rankL0
    rankL0 = rank0 bv l

-- | \(O(\log n)\) Given an interval \([l, r)\), it returns the index of the first occurrence
-- (0-based) of \(1\) in the sequence, or `Nothing` if no such occurrence exists.
--
-- @since 1.1.0.0
{-# INLINE selectKthIn1 #-}
selectKthIn1 ::
  -- | A bit vector
  BitVector ->
  -- | \(l\)
  Int ->
  -- | \(r\)
  Int ->
  -- | \(k\)
  Int ->
  -- | The index of \(k\)-th \(1\) in \([l, r)\)
  Maybe Int
selectKthIn1 bv l r k
  | k < 0 || nOnes <= k = Nothing
  -- note that `rank1` takes exclusive index
  | otherwise = Just . maxRight l r $ \i -> rank1 bv (i + 1) - rankL1 < k + 1
  where
    nOnes = rank1 bv r - rankL1
    rankL1 = rank1 bv l
