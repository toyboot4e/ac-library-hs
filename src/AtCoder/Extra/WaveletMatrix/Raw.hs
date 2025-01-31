{-# LANGUAGE RecordWildCards #-}

-- original implementation:
-- <https://miti-7.hatenablog.com/entry/2018/04/28/152259>

-- NOTE: We could integrate cumulative sum / fenwick tree / segment tree.
-- NOTE: `topK` and `intersects` are not implemented as they are slow.

-- | A static Wavelet Matrix without automatic index comperssion. Consider using
-- @AtCoder.Extra.WaveletMatrix@ instead.
--
-- @since 1.1.0.0
module AtCoder.Extra.WaveletMatrix.Raw
  ( -- * RawWaveletMatrix
    RawWaveletMatrix (..),

    -- * Constructors
    build,

    -- * Access (indexing)
    access,

    -- * rank
    rankLT,
    rank,
    rankBetween,

    -- * Select
    select,
    selectKth,
    selectIn,
    selectKthIn,

    -- * Quantile (value-ordered access)

    -- ** Safe (total)
    kthSmallestIn,
    ikthSmallestIn,
    kthLargestIn,
    ikthLargestIn,

    -- ** Unsafe
    unsafeKthSmallestIn,
    unsafeIKthSmallestIn,
    unsafeKthLargestIn,
    unsafeIKthLargestIn,

    -- * Lookup
    lookupLE,
    lookupLT,
    lookupGE,
    lookupGT,

    -- * Conversions
    assocsIn,
    assocsWith,
    descAssocsIn,
    descAssocsInWith,
  )
where

import AtCoder.Extra.WaveletMatrix.BitVector qualified as BV
import AtCoder.Internal.Assert qualified as ACIA
import AtCoder.Internal.Bit qualified as ACIB
import Control.Monad.ST (runST)
import Data.Bit (Bit (..))
import Data.Bits (bit, countTrailingZeros, setBit, testBit, (.|.))
import Data.Maybe
import Data.Vector qualified as V
import Data.Vector.Algorithms.Radix qualified as VAR
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Stack (HasCallStack)

-- | A static Wavelet Matrix without automatic index comperssion.
--
-- @since 1.1.0.0
data RawWaveletMatrix = RawWaveletMatrix
  { -- | \(\lceil \log_2 N \rceil\).
    --
    -- @since 1.1.0.0
    heightRwm :: {-# UNPACK #-} !Int,
    -- | The length of the original array.
    --
    -- @since 1.1.0.0
    lengthRwm :: {-# UNPACK #-} !Int,
    -- | The bit matrix. Each row represents (heightRwm - 1 - iRow) bit's on/off.
    --
    -- @since 1.1.0.0
    bitsRwm :: !(V.Vector BV.BitVector),
    -- | The number of zeros bits in each row in the bit matrix.
    --
    -- @since 1.1.0.0
    nZerosRwm :: !(VU.Vector Int)
  }
  deriving (Eq, Show)

-- | \(O(n \log n)\) Creates a `RawWaveletMatrix` from a vector \(a\).
--
-- @since 1.1.0.0
{-# INLINEABLE build #-}
build ::
  (HasCallStack) =>
  -- | The number of different values in the compressed vector.
  Int ->
  -- | A compressed vector
  VU.Vector Int ->
  -- | A wavelet matrix
  RawWaveletMatrix
build nx xs
  | nx < 0 = error "AtCoder.Extra.WaveletMatrix.Raw.build: given negative `n`"
  | otherwise = runST $ do
      -- TODO: less mutable variables
      orgBits <- VUM.replicate (lengthRwm * heightRwm) $ Bit False
      orgCsum <- VUM.replicate (lenCSum * heightRwm) (0 :: Int)
      nZeros <- VUM.unsafeNew heightRwm

      -- views by row over the contiguous memory:
      let !bits = V.unfoldrExactN heightRwm (VUM.splitAt lengthRwm) orgBits
      let !csums = V.unfoldrExactN heightRwm (VUM.splitAt lenCSum) orgCsum

      -- the vector will be sorted by bits.
      vec <- VU.thaw xs
      V.izipWithM_
        ( \iRow bitVec csum -> do
            let !iBit = heightRwm - 1 - iRow
            vec' <- VU.unsafeFreeze vec
            VU.iforM_ vec' $ \i x -> do
              VGM.unsafeWrite bitVec i . Bit $ testBit x iBit

            -- csum.
            VGM.unsafeWrite csum 0 (0 :: Int)
            bitVec' <- VU.unsafeFreeze bitVec

            -- get popCount by word. TODO: use `castToWords` for most elements
            nOnes <- BV.csumInPlace csum bitVec'
            VGM.unsafeWrite nZeros iRow (lengthRwm - nOnes)

            -- preform a stable sort by the bit:
            VAR.sortBy 2 2 (\_ x -> fromEnum (testBit x iBit)) vec
        )
        bits
        csums
      nZerosRwm <- VU.unsafeFreeze nZeros
      bits' <- V.unfoldrExactN heightRwm (VU.splitAt lengthRwm) <$> VU.unsafeFreeze orgBits
      csums' <- V.unfoldrExactN heightRwm (VU.splitAt lenCSum) <$> VU.unsafeFreeze orgCsum
      let !bitsRwm = V.zipWith BV.BitVector bits' csums'
      pure $ RawWaveletMatrix {..}
  where
    !lengthRwm = VG.length xs
    !lenCSum = (lengthRwm + BV.wordSize - 1) `div` BV.wordSize + 1 -- +1 for the zero
    !heightRwm = countTrailingZeros $ ACIB.bitCeil nx

-- | \(O(\log |S|)\) Returns \(a[k]\) or `Nothing` if the index is out of the bounds. Try to use the
-- original array if you can.
--
-- @since 1.1.0.0
{-# INLINEABLE access #-}
access :: RawWaveletMatrix -> Int -> Maybe Int
access RawWaveletMatrix {..} i0
  | ACIA.testIndex i0 lengthRwm =
      let (!_, !res) =
            V.ifoldl'
              ( \(!i, !acc) !iRow !bits ->
                  let Bit !goRight = VG.unsafeIndex (BV.bitsBv bits) i
                      !i'
                        | goRight = BV.rank1 bits i + VG.unsafeIndex nZerosRwm iRow
                        | otherwise = BV.rank0 bits i
                      !acc'
                        | goRight = setBit acc (heightRwm - 1 - iRow)
                        | otherwise = acc
                   in (i', acc')
              )
              (i0, 0)
              bitsRwm
       in Just res
  | otherwise = Nothing

-- | \(O(\log |A|)\) Goes down the wavelet matrix for collecting the kth smallest value.
--
-- @since 1.1.0.0
{-# INLINEABLE goDown #-}
goDown :: RawWaveletMatrix -> Int -> Int -> Int -> (Int, Int, Int, Int)
goDown RawWaveletMatrix {..} l_ r_ k_ = V.ifoldl' step (0 :: Int, l_, r_, k_) bitsRwm
  where
    -- It's binary search over the value range. In each row, we'll focus on either 0 bit values or
    -- 1 bit values in [l, r) and update the range to [l', r').
    step (!acc, !l, !r, !k) !iRow !bits
      -- `r0 - l0`, the number of zeros in [l, r), is bigger than or equal to k:
      -- Go left.
      | k < r0 - l0 = (acc, l0, r0, k)
      -- Go right.
      | otherwise =
          let !acc' = acc .|. bit (heightRwm - 1 - iRow)
              !nZeros = VG.unsafeIndex nZerosRwm iRow
              -- every zero bits come to the left after the move.
              !l' = l + nZeros - l0 -- add the number of zeros in [0, l)
              !r' = r + nZeros - r0 -- add the number of zeros in [0, r)
              !k' = k - (r0 - l0) -- `r0 - l0` zeros go left
           in (acc', l', r', k')
      where
        !l0 = BV.rank0 bits l
        !r0 = BV.rank0 bits r

-- | \(O(\log |A|)\) Goes up the wavelet matrix for collecting the value \(x\).
--
-- @since 1.1.0.0
{-# INLINEABLE goUp #-}
goUp :: RawWaveletMatrix -> Int -> Int -> Maybe Int
goUp RawWaveletMatrix {..} i0 x =
  V.ifoldM'
    ( \ !i !iBit !bits ->
        if testBit x iBit
          then BV.select1 bits $ i - nZerosRwm VG.! (heightRwm - 1 - iBit)
          else BV.select0 bits i
    )
    i0
    (V.reverse bitsRwm)

-- | \(O(\log |S|)\) Returns the number of \(y\) in \([l, r) \times [0, y_0)\).
--
-- @since 1.1.0.0
{-# INLINEABLE rankLT #-}
rankLT :: RawWaveletMatrix -> Int -> Int -> Int -> Int
rankLT RawWaveletMatrix {..} l_ r_ xr
  -- REMARK: This is required. The function below cannot handle the case N = 2^i and xr = N.
  | xr >= bit heightRwm = r'_ - l'_
  | xr <= 0 = 0
  | r'_ <= l'_ = 0
  | otherwise =
      let (!res, !_, !_) = V.ifoldl' step (0, l'_, r'_) bitsRwm
       in res
  where
    -- clamp
    l'_ = max 0 l_
    r'_ = min lengthRwm r_
    -- [l, r)
    step (!acc, !l, !r) !iRow !bits =
      let !b = testBit xr (heightRwm - 1 - iRow)
          !l0 = BV.rank0 bits l
          !r0 = BV.rank0 bits r
       in if b
            then (acc + r0 - l0, l - l0 + VG.unsafeIndex nZerosRwm iRow, r - r0 + VG.unsafeIndex nZerosRwm iRow)
            else (acc, l0, r0)

-- | \(O(\log |S|)\) Returns the number of \(y\) in \([l, r)\).
--
-- @since 1.1.0.0
{-# INLINEABLE rank #-}
rank ::
  RawWaveletMatrix ->
  -- | \(l\)
  Int ->
  -- | \(r\)
  Int ->
  -- | \(y\)
  Int ->
  -- | The number of \(y\) in \([l, r)\).
  Int
rank wm l r x = rankBetween wm l r x (x + 1)

-- | \(O(\log |S|)\) Returns the number of \(y\) in \([l, r) \times [y_1, y_2)\).
--
-- @since 1.1.0.0
{-# INLINEABLE rankBetween #-}
rankBetween ::
  RawWaveletMatrix ->
  -- | \(l\)
  Int ->
  -- | \(r\)
  Int ->
  -- | \(y_1\)
  Int ->
  -- | \(y_2\)
  Int ->
  -- | The number of \(y\) in \([l, r) \times [y_1, y_2)\).
  Int
rankBetween wm l r lx rx = rankLT wm l r rx - rankLT wm l r lx

-- | \(O(\log |S|)\) Returns the index of the first \(y\) in the sequence, or `Nothing` if \(y\) is
-- not found.
--
-- @since 1.1.0.0
{-# INLINEABLE select #-}
select :: RawWaveletMatrix -> Int -> Maybe Int
select wm = selectKth wm 0

-- | \(O(\log |S|)\) Returns the index of the \(k\)-th occurrence (0-based) of \(y\), or `Nothing`
-- if no such occurrence exists.
--
-- @since 1.1.0.0
{-# INLINEABLE selectKth #-}
selectKth ::
  RawWaveletMatrix ->
  -- | \(k\)
  Int ->
  -- | \(y\)
  Int ->
  -- | The index of \(k\)-th \(y\)
  Maybe Int
selectKth wm = selectKthIn wm 0 (lengthRwm wm)

-- | \(O(\log |S|)\) Given an interval \([l, r)\), it returns the index of the first occurrence
-- (0-based) of \(y\) in the sequence, or `Nothing` if no such occurrence exists.
--
-- @since 1.1.0.0
{-# INLINEABLE selectIn #-}
selectIn ::
  -- | A wavelet matrix
  RawWaveletMatrix ->
  -- | \(l\)
  Int ->
  -- | \(r\)
  Int ->
  -- | \(k\)
  Int ->
  -- | The index of the first \(y\) in \([l, r)\).
  Maybe Int
selectIn wm = selectKthIn wm 0

-- | \(O(\log |S|)\) Given an interval \([l, r)\), it returns the index of the \(k\)-th occurrence
-- (0-based) of \(y\) in the sequence, or `Nothing` if no such occurrence exists.
--
-- @since 1.1.0.0
{-# INLINEABLE selectKthIn #-}
selectKthIn ::
  RawWaveletMatrix ->
  -- | \(l\)
  Int ->
  -- | \(r\)
  Int ->
  -- | \(k\)
  Int ->
  -- | \(y\)
  Int ->
  -- | The index of the \(k\)-th \(y\) in \([l, r)\).
  Maybe Int
selectKthIn wm@RawWaveletMatrix {..} l_ r_ k x
  | not (0 <= x && x < lengthRwm && 0 <= k && k < lengthRwm) = Nothing
  | l'_ < r'_ = inner
  | otherwise = Nothing
  where
    -- clamp
    l'_ = max 0 l_
    r'_ = min lengthRwm r_
    inner :: Maybe Int
    inner
      | rEnd <= lEnd + k = Nothing
      -- go up
      | otherwise = goUp wm (lEnd + k) x
      where
        -- TODO: replace with goDown
        -- Go down. Gets the [l, r) range of @x@ in the last array.
        (!lEnd, !rEnd) =
          V.ifoldl'
            ( \(!l, !r) !iRow !bits ->
                let !l0 = BV.rank0 bits l
                    !r0 = BV.rank0 bits r
                 in if testBit x (heightRwm - 1 - iRow)
                      then (l + nZerosRwm VG.! iRow - l0, r + nZerosRwm VG.! iRow - r0)
                      else (l0, r0)
            )
            (l'_, r'_)
            bitsRwm

-- | \(O(\log |S|)\) Given an interval \([l, r)\), it returns the index of the \(k\)-th (0-based)
-- largest value. Note that duplicated values are counted as distinct occurrences.
--
-- @since 1.1.0.0
{-# INLINEABLE kthLargestIn #-}
kthLargestIn ::
  -- | A wavelet matrix
  RawWaveletMatrix ->
  -- | \(l\)
  Int ->
  -- | \(r\)
  Int ->
  -- | \(k\)
  Int ->
  -- | \(k\)-th largest \(y\) in \([l, r)\)
  Maybe Int
kthLargestIn wm l r k
  | k < 0 || k >= r - l = Nothing
  | 0 <= l && l < r && r <= lengthRwm wm = Just $ unsafeKthLargestIn wm l r k
  | otherwise = Nothing

-- | \(O(\log |S|)\) Given an interval \([l, r)\), it returns both the index and the value of the
-- \(k\)-th (0-based) largest value. Note that duplicated values are counted as distinct occurrences.
--
-- @since 1.1.0.0
{-# INLINEABLE ikthLargestIn #-}
ikthLargestIn ::
  -- | A wavelet matrix
  RawWaveletMatrix ->
  -- | \(l\)
  Int ->
  -- | \(r\)
  Int ->
  -- | \(k\)
  Int ->
  -- | \((i, y)\) for \(k\)-th largest \(y\) in \([l, r)\)
  Maybe (Int, Int)
ikthLargestIn wm l r k
  | k < 0 || k >= r - l = Nothing
  | 0 <= l && l < r && r <= lengthRwm wm = Just $ unsafeIKthLargestIn wm l r k
  | otherwise = Nothing

-- | \(O(\log |S|)\) Given an interval \([l, r)\), it returns the index of the \(k\)-th (0-based)
-- smallest value. Note that duplicated values are counted as distinct occurrences.
--
-- @since 1.1.0.0
{-# INLINEABLE kthSmallestIn #-}
kthSmallestIn ::
  -- | A wavelet matrix
  RawWaveletMatrix ->
  -- | \(l\)
  Int ->
  -- | \(r\)
  Int ->
  -- | \(k\)
  Int ->
  -- | \(k\)-th largest \(y\) in \([l, r)\)
  Maybe Int
kthSmallestIn wm l r k
  | k < 0 || k >= r - l = Nothing
  | 0 <= l && l < r && r <= lengthRwm wm = Just $ unsafeKthSmallestIn wm l r k
  | otherwise = Nothing

-- | \(O(\log |S|)\) Given an interval \([l, r)\), it returns both the index and the value of the
-- \(k\)-th (0-based) smallest value. Note that duplicated values are counted as distinct occurrences.
--
-- @since 1.1.0.0
{-# INLINEABLE ikthSmallestIn #-}
ikthSmallestIn ::
  RawWaveletMatrix ->
  -- | \(l\)
  Int ->
  -- | \(r\)
  Int ->
  -- | \(k\)
  Int ->
  -- | \((i, y)\) for \(k\)-th largest \(y\) in \([l, r)\)
  Maybe (Int, Int)
ikthSmallestIn wm l r k
  | k < 0 || k >= r - l = Nothing
  | 0 <= l && l < r && r <= lengthRwm wm = Just $ unsafeIKthSmallestIn wm l r k
  | otherwise = Nothing

-- | \(O(\log a)\) Returns \(k\)-th (0-based) biggest number in \([l, r)\). Note that duplicated
-- values are counted as distinct occurrences.
--
-- @since 1.1.0.0
{-# INLINEABLE unsafeKthLargestIn #-}
unsafeKthLargestIn :: RawWaveletMatrix -> Int -> Int -> Int -> Int
unsafeKthLargestIn wm l r k = unsafeKthSmallestIn wm l r (r - l - (k + 1))

-- | \(O(\log a)\)
--
-- @since 1.1.0.0
{-# INLINEABLE unsafeIKthLargestIn #-}
unsafeIKthLargestIn :: RawWaveletMatrix -> Int -> Int -> Int -> (Int, Int)
unsafeIKthLargestIn wm l r k = unsafeIKthSmallestIn wm l r (r - l - (k + 1))

-- | \(O(\log a)\)
--
-- @since 1.1.0.0
{-# INLINEABLE unsafeKthSmallestIn #-}
unsafeKthSmallestIn :: RawWaveletMatrix -> Int -> Int -> Int -> Int
unsafeKthSmallestIn wm l_ r_ k_ =
  let (!x, !_, !_, !_) = goDown wm l_ r_ k_
   in x

-- | \(O(\log a)\)
--
-- @since 1.1.0.0
{-# INLINEABLE unsafeIKthSmallestIn #-}
unsafeIKthSmallestIn :: RawWaveletMatrix -> Int -> Int -> Int -> (Int, Int)
unsafeIKthSmallestIn wm l_ r_ k_ =
  let (!x, !l, !_, !k) = goDown wm l_ r_ k_
      !i' = fromJust $ goUp wm (l + k) x
   in (i', x)

-- | \(O(\log |S|)\) Looks up the maximum \(y\) in \([l, r) \times (-\infty, y_0]\).
--
-- @since 1.1.0.0
{-# INLINEABLE lookupLE #-}
lookupLE ::
  -- | A wavelet matrix
  RawWaveletMatrix ->
  -- | \(l\)
  Int ->
  -- | \(r\)
  Int ->
  -- | \(y_0\)
  Int ->
  -- | Maximum \(y\) in \([l, r) \times (-\infty, y_0]\)
  Maybe Int
lookupLE wm l r x
  | r' == l' = Nothing
  | rank_ == 0 = Nothing
  | otherwise = Just $ unsafeKthSmallestIn wm l' r' (rank_ - 1)
  where
    -- clamp
    l' = max 0 l
    r' = min (lengthRwm wm) r
    rank_ = rankBetween wm l r minBound (x + 1)

-- | \(O(\log a)\) Finds the maximum \(x\) in \([l, r)\) s.t. \(x_{0} \lt x\).
--
-- @since 1.1.0.0
{-# INLINEABLE lookupLT #-}
lookupLT ::
  RawWaveletMatrix ->
  -- | \(l\)
  Int ->
  -- | \(r\)
  Int ->
  -- | \(x\)
  Int ->
  -- | Maximum \(y\) in \([l, r) \times (-\infty, y_0)\)
  Maybe Int
lookupLT wm l r x = lookupLE wm l r (x - 1)

-- | \(O(\log |S|)\) Looks up the minimum \(y\) in \([l, r) \times [y_0, \infty)\).
--
-- @since 1.1.0.0
{-# INLINEABLE lookupGE #-}
lookupGE ::
  RawWaveletMatrix ->
  -- | \(l\)
  Int ->
  -- | \(r\)
  Int ->
  -- | \(y_0\)
  Int ->
  -- | Minimum \(y\) in \([l, r) \times [y_0, \infty)\).
  Maybe Int
lookupGE wm l r x
  | r' == l' = Nothing
  | rank_ >= r' - l' = Nothing
  | otherwise =
      Just $ unsafeKthSmallestIn wm l' r' rank_
  where
    -- clamp
    l' = max 0 l
    r' = min (lengthRwm wm) r
    rank_ = rankBetween wm l' r' minBound x

-- | \(O(\log |S|)\) Looks up the minimum \(y\) in \([l, r) \times (y_0, \infty)\).
--
-- @since 1.1.0.0
{-# INLINEABLE lookupGT #-}
lookupGT ::
  RawWaveletMatrix ->
  -- | \(l\)
  Int ->
  -- | \(r\)
  Int ->
  -- | \(y_0\)
  Int ->
  -- | Minimum \(y\) in \([l, r) \times (y_0, \infty)\)
  Maybe Int
lookupGT wm l r x = lookupGE wm l r (x + 1)

-- | \(O(\min(|S|, L) \log |S|)\) Collects \((y, \mathrm{rank}(y))\) in range \([l, r)\) in
-- ascending order of \(y\). Note that it's only fast when the \(|S|\) is very small.
--
-- @since 1.1.0.0
{-# INLINEABLE assocsIn #-}
assocsIn :: RawWaveletMatrix -> Int -> Int -> [(Int, Int)]
assocsIn wm l r = assocsWith wm l r id

-- | \(O(\log A \min(|A|, L))\) Internal implementation of `assocs`.
--
-- @since 1.1.0.0
{-# INLINEABLE assocsWith #-}
assocsWith :: RawWaveletMatrix -> Int -> Int -> (Int -> Int) -> [(Int, Int)]
assocsWith RawWaveletMatrix {..} l_ r_ f
  | l'_ < r'_ = inner (0 :: Int) (0 :: Int) l'_ r'_ []
  | otherwise = []
  where
    -- clamp
    l'_ = max 0 l_
    r'_ = min lengthRwm r_
    -- DFS. [l, r)
    inner !acc iRow !l !r res
      | iRow >= heightRwm =
          let !n = r - l
              !acc' = f acc
           in (acc', n) : res
      | otherwise = do
          let !bits = bitsRwm VG.! iRow
              !l0 = BV.rank0 bits l
              !r0 = BV.rank0 bits r
              !nZeros = nZerosRwm VG.! iRow
              -- go right (visit bigger values first)
              !l' = l + nZeros - l0
              !r' = r + nZeros - r0
              !res'
                | l' < r' = inner (acc .|. bit (heightRwm - 1 - iRow)) (iRow + 1) l' r' res
                | otherwise = res
              !res''
                -- go left
                | l0 < r0 = inner acc (iRow + 1) l0 r0 res'
                | otherwise = res'
           in res''

-- | \(O(\min(|S|, L) \log |S|)\) Collects \((y, \mathrm{rank}(y))\) in range \([l, r)\) in
-- descending order of \(y\). Note that it's only fast when the \(|S|\) is very small.
--
-- @since 1.1.0.0
{-# INLINEABLE descAssocsIn #-}
descAssocsIn :: RawWaveletMatrix -> Int -> Int -> [(Int, Int)]
descAssocsIn wm l r = descAssocsInWith wm l r id

-- | \(O(\log A \min(|A|, L))\) Internal implementation of `descAssoc`.
--
-- @since 1.1.0.0
{-# INLINEABLE descAssocsInWith #-}
descAssocsInWith :: RawWaveletMatrix -> Int -> Int -> (Int -> Int) -> [(Int, Int)]
descAssocsInWith RawWaveletMatrix {..} l_ r_ f
  | l'_ < r'_ = inner (0 :: Int) (0 :: Int) l'_ r'_ []
  | otherwise = []
  where
    -- clamp
    l'_ = max 0 l_
    r'_ = min lengthRwm r_
    -- DFS. [l, r)
    inner !acc iRow !l !r res
      | iRow >= heightRwm =
          let !n = r - l
              !acc' = f acc
           in (acc', n) : res
      | otherwise = do
          let !bits = bitsRwm VG.! iRow
              !l0 = BV.rank0 bits l
              !r0 = BV.rank0 bits r
              !nZeros = nZerosRwm VG.! iRow
              !res'
                -- go left
                | l0 < r0 = inner acc (iRow + 1) l0 r0 res
                | otherwise = res
              -- go right (visit bigger values first)
              !l' = l + nZeros - l0
              !r' = r + nZeros - r0
              !res''
                | l' < r' = inner (acc .|. bit (heightRwm - 1 - iRow)) (iRow + 1) l' r' res'
                | otherwise = res'
           in res''
