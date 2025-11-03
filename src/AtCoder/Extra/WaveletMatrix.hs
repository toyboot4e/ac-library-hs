{-# LANGUAGE RecordWildCards #-}

-- | A static Wavelet Matrix.
--
-- ==== Notation
-- Let \(S\) be the set of values in your wavelet matrix. We use \(|S|\) to denote the number of
-- distinct values contained within this set \((|S| \lt n)\).
--
-- @since 1.1.0.0
module AtCoder.Extra.WaveletMatrix
  ( -- * Wavelet Matrix
    WaveletMatrix (..),

    -- * Constructors
    build,

    -- * Access (indexing)
    access,

    -- * Rank (count)
    rank,
    rankBetween,

    -- * Selection (finding index)

    -- | ==== __Example__
    -- >>> import AtCoder.Extra.WaveletMatrix qualified as WM
    -- >>> import Data.Vector.Unboxed qualified as VU
    -- >>> let wm = WM.build $ VU.fromList [1,1,2,1,3]
    -- >>> WM.select wm 1
    -- Just 0
    -- >>> WM.selectKth wm 2 1
    -- Just 3
    -- >>> WM.selectIn wm {- [l, r) -} 1 4 {- x -} 1
    -- Just 1
    -- >>> WM.selectKthIn wm {- [l, r) -} 1 4 {- k -} 1 {- x -} 1
    -- Just 3
    select,
    selectKth,
    selectIn,
    selectKthIn,

    -- * Quantile (value-ordered access)
    kthLargestIn,
    ikthLargestIn,
    kthSmallestIn,
    ikthSmallestIn,
    -- unsafeKthLargestIn,
    -- unsafeIKthLargestIn,
    -- unsafeKthSmallestIn,
    -- unsafeIKthSmallestIn,

    -- * Lookup
    lookupLE,
    lookupLT,
    lookupGE,
    lookupGT,

    -- * Conversions
    assocsIn,
    descAssocsIn,
  )
where

import AtCoder.Extra.Bisect (lowerBound)
import AtCoder.Extra.WaveletMatrix.Raw qualified as Rwm
import Control.Monad
import Data.Vector.Algorithms.Intro qualified as VAI
import Data.Vector.Generic qualified as VG
import Data.Vector.Unboxed qualified as VU
import GHC.Stack (HasCallStack)

-- | A static Wavelet Matrix.
--
-- @since 1.3.0.0
data WaveletMatrix = WaveletMatrix
  { -- | The internal wavelet matrix, where index compression is not automatically performed.
    --
    -- @since 1.3.0.0
    rawWm :: !Rwm.RawWaveletMatrix,
    -- | Index compression dictionary.
    --
    -- @since 1.3.0.0
    yDictWm :: !(VU.Vector Int)
  }

-- | \(O(n \log n)\) Creates a `WaveletMatrix` from an array \(a\).
--
-- @since 1.1.0.0
{-# INLINE build #-}
build :: VU.Vector Int -> WaveletMatrix
build ys =
  let !yDictWm = VU.uniq $ VU.modify (VAI.sortBy compare) ys
      !ys' = VU.map (lowerBound yDictWm) ys
      !rawWm = Rwm.build (VG.length ys) ys'
   in WaveletMatrix {..}

-- | \(O(\log |S|)\) Returns \(a[k]\) or `Nothing` if the index is out of the bounds. Try to use the
-- original array if you can.
--
-- @since 1.1.0.0
{-# INLINE access #-}
access :: WaveletMatrix -> Int -> Maybe Int
access WaveletMatrix {..} i = (yDictWm VG.!) <$> Rwm.access rawWm i

-- | \(O(\log |S|)\) Returns the number of \(y\) in \([l, r)\).
--
-- @since 1.1.0.0
{-# INLINE rank #-}
rank ::
  -- | A wavelet matrix
  WaveletMatrix ->
  -- | \(l\)
  Int ->
  -- | \(r\)
  Int ->
  -- | \(y\)
  Int ->
  -- | The number of \(y\) in \([l, r)\).
  Int
rank wm l r y = rankBetween wm l r y (y + 1)

-- | \(O(\log |S|)\) Returns the number of \(y\) in \([l, r) \times [y_1, y_2)\).
--
-- @since 1.1.0.0
{-# INLINEABLE rankBetween #-}
rankBetween ::
  -- | A wavelet matrix
  WaveletMatrix ->
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
rankBetween WaveletMatrix {..} l r y1 y2
  | not $ 0 <= l && l < r && r <= n = 0
  | y1' >= y2' = 0
  | otherwise = Rwm.rankBetween rawWm l r y1' y2'
  where
    -- Handles the case @yl@ or  @yr@ is not in the dict
    n = Rwm.lengthRwm rawWm
    y1' = lowerBound yDictWm y1
    y2' = lowerBound yDictWm y2

-- | \(O(\log |S|)\) Returns the index of the first \(y\) in \(a\), or `Nothing` if \(y\) is
-- not found.
--
-- @since 1.1.0.0
{-# INLINE select #-}
select :: WaveletMatrix -> Int -> Maybe Int
select wm = selectKth wm 0

-- | \(O(\log |S|)\) Returns the index of the \(k\)-th occurrence (0-based) of \(y\), or `Nothing`
-- if no such occurrence exists.
--
-- @since 1.1.0.0
{-# INLINEABLE selectKth #-}
selectKth ::
  -- | A wavelet matrix
  WaveletMatrix ->
  -- | \(k\)
  Int ->
  -- | \(y\)
  Int ->
  -- | The index of \(k\)-th \(y\)
  Maybe Int
selectKth WaveletMatrix {..} k y = do
  let !i = lowerBound yDictWm y
  guard $ i < VG.length yDictWm
  -- TODO: we don't need such an explicit branch?
  let !y' = yDictWm VG.! i
  guard $ y' == y
  Rwm.selectKth rawWm k i

-- | \(O(\log |S|)\) Given an interval \([l, r)\), it returns the index of the first occurrence
-- (0-based) of \(y\) in the sequence, or `Nothing` if no such occurrence exists.
--
-- @since 1.1.0.0
{-# INLINE selectIn #-}
selectIn ::
  -- | A wavelet matrix
  WaveletMatrix ->
  -- | \(l\)
  Int ->
  -- | \(r\)
  Int ->
  -- | \(y\)
  Int ->
  -- | The index of the first \(y\) in \([l, r)\).
  Maybe Int
selectIn wm l r = selectKthIn wm l r 0

-- | \(O(\log |S|)\) Given an interval \([l, r)\), it returns the index of the \(k\)-th occurrence
-- (0-based) of \(y\) in the sequence, or `Nothing` if no such occurrence exists.
--
-- @since 1.1.0.0
{-# INLINEABLE selectKthIn #-}
selectKthIn ::
  (HasCallStack) =>
  -- | A wavelet matrix
  WaveletMatrix ->
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
selectKthIn WaveletMatrix {..} l r k y = do
  let !i = lowerBound yDictWm y
  guard $ i < VG.length yDictWm
  -- TODO: we don't need such an explicit branch?
  let !y' = yDictWm VG.! i
  guard $ y' == y
  Rwm.selectKthIn rawWm l r k i

-- | \(O(\log |S|)\) Given the interval \([l, r)\), returns the index of the \(k\)-th (0-based)
-- largest value in it. Note that duplicated values are treated as distinct occurrences.
--
-- @since 1.1.0.0
{-# INLINEABLE kthLargestIn #-}
kthLargestIn ::
  (HasCallStack) =>
  -- | A wavelet matrix
  WaveletMatrix ->
  -- | \(l\)
  Int ->
  -- | \(r\)
  Int ->
  -- | \(k\)
  Int ->
  -- | \(k\)-th largest \(y\) in \([l, r)\)
  Maybe Int
kthLargestIn WaveletMatrix {..} l r k
  | Just !y <- Rwm.kthLargestIn rawWm l r k = Just $ yDictWm VG.! y
  | otherwise = Nothing

-- | \(O(\log |S|)\) Given the interval \([l, r)\), returns both the index and the value of the
-- \(k\)-th (0-based) largest value in it. Note that duplicated values are treated as distinct
-- occurrences.
--
-- @since 1.1.0.0
{-# INLINEABLE ikthLargestIn #-}
ikthLargestIn ::
  -- | A wavelet matrix
  WaveletMatrix ->
  -- | \(l\)
  Int ->
  -- | \(r\)
  Int ->
  -- | \(k\)
  Int ->
  -- | \((i, y)\) for \(k\)-th largest \(y\) in \([l, r)\)
  Maybe (Int, Int)
ikthLargestIn WaveletMatrix {..} l r k
  | Just (!i, !y) <- Rwm.ikthLargestIn rawWm l r k = Just (i, yDictWm VG.! y)
  | otherwise = Nothing

-- | \(O(\log |S|)\) Given the interval \([l, r)\), returns the index of the \(k\)-th (0-based)
-- smallest value in it. Note that duplicated values are treated as distinct occurrences.
--
-- @since 1.1.0.0
{-# INLINEABLE kthSmallestIn #-}
kthSmallestIn ::
  -- | A wavelet matrix
  WaveletMatrix ->
  -- | \(l\)
  Int ->
  -- | \(r\)
  Int ->
  -- | \(k\)
  Int ->
  -- | \(k\)-th largest \(y\) in \([l, r)\)
  Maybe Int
kthSmallestIn WaveletMatrix {..} l r k
  | Just !y <- Rwm.kthSmallestIn rawWm l r k = Just $ yDictWm VG.! y
  | otherwise = Nothing

-- | \(O(\log |S|)\) Given the interval \([l, r)\), returns both the index and the value of the
-- \(k\)-th (0-based) smallest value in it. Note that duplicated values are treated as distinct
-- occurrences.
--
-- @since 1.1.0.0
{-# INLINEABLE ikthSmallestIn #-}
ikthSmallestIn ::
  WaveletMatrix ->
  -- | \(l\)
  Int ->
  -- | \(r\)
  Int ->
  -- | \(k\)
  Int ->
  -- | \((i, y)\) for \(k\)-th largest \(y\) in \([l, r)\)
  Maybe (Int, Int)
ikthSmallestIn WaveletMatrix {..} l r k
  | Just (!i, !y) <- Rwm.ikthSmallestIn rawWm l r k = Just (i, yDictWm VG.! y)
  | otherwise = Nothing

-- | \(O(\log |S|)\)
--
-- @since 1.1.0.0
{-# INLINE unsafeKthSmallestIn #-}
unsafeKthSmallestIn :: WaveletMatrix -> Int -> Int -> Int -> Int
unsafeKthSmallestIn WaveletMatrix {..} l r k =
  yDictWm VG.! Rwm.unsafeKthSmallestIn rawWm l r k

-- | \(O(\log |S|)\) Looks up the maximum \(y\) in \([l, r) \times (-\infty, y_0]\).
--
-- @since 1.1.0.0
{-# INLINEABLE lookupLE #-}
lookupLE ::
  -- | A wavelet matrix
  WaveletMatrix ->
  -- | \(l\)
  Int ->
  -- | \(r\)
  Int ->
  -- | \(y_0\)
  Int ->
  -- | Maximum \(y\) in \([l, r) \times (-\infty, y_0]\)
  Maybe Int
lookupLE wm l r y0
  | r' == l' = Nothing
  | rank_ == 0 = Nothing
  | otherwise = Just $ unsafeKthSmallestIn wm l' r' (rank_ - 1)
  where
    -- clamp
    l' = max 0 l
    r' = min (Rwm.lengthRwm (rawWm wm)) r
    rank_ = rankBetween wm l' r' minBound (y0 + 1)

-- | \(O(\log |S|)\) Looks up the maximum \(y\) in \([l, r) \times (-\infty, y_0)\).
--
-- @since 1.1.0.0
{-# INLINE lookupLT #-}
lookupLT ::
  -- | A wavelet matrix
  WaveletMatrix ->
  -- | \(l\)
  Int ->
  -- | \(r\)
  Int ->
  -- | \(y_0\)
  Int ->
  -- | Maximum \(y\) in \([l, r) \times (-\infty, y_0)\)
  Maybe Int
lookupLT wm l r y0 = lookupLE wm l r (y0 - 1)

-- | \(O(\log |S|)\) Looks up the minimum \(y\) in \([l, r) \times [y_0, \infty)\).
--
-- @since 1.1.0.0
{-# INLINEABLE lookupGE #-}
lookupGE ::
  -- | A wavelet matrix
  WaveletMatrix ->
  -- | \(l\)
  Int ->
  -- | \(r\)
  Int ->
  -- | \(y_0\)
  Int ->
  -- | Minimum \(y\) in \([l, r) \times [y_0, \infty)\).
  Maybe Int
lookupGE wm l r y0
  | r' == l' = Nothing
  | rank_ >= r - l = Nothing
  | otherwise = Just $ unsafeKthSmallestIn wm l r rank_
  where
    -- clamp
    l' = max 0 l
    r' = min (Rwm.lengthRwm (rawWm wm)) r
    rank_ = rankBetween wm l' r' minBound y0

-- | \(O(\log |S|)\) Looks up the minimum \(y\) in \([l, r) \times (y_0, \infty)\).
--
-- @since 1.1.0.0
{-# INLINE lookupGT #-}
lookupGT ::
  -- | A wavelet matrix
  WaveletMatrix ->
  -- | \(l\)
  Int ->
  -- | \(r\)
  Int ->
  -- | \(y_0\)
  Int ->
  -- | Minimum \(y\) in \([l, r) \times (y_0, \infty)\)
  Maybe Int
lookupGT wm l r y0 = lookupGE wm l r (y0 + 1)

-- | \(O(\min(|S|, L) \log |S|)\) Collects \((y, \mathrm{rank}(y))\) in an interval \([l, r)\) in
-- ascending order of \(y\). Note that it's only fast when the \(|S|\) is very small.
--
-- @since 1.1.0.0
{-# INLINE assocsIn #-}
assocsIn :: WaveletMatrix -> Int -> Int -> [(Int, Int)]
assocsIn WaveletMatrix {..} l r = Rwm.assocsWith rawWm l r (yDictWm VG.!)

-- | \(O(\min(|S|, L) \log |S|)\) Collects \((y, \mathrm{rank}(y))\) in an interval \([l, r)\) in
-- descending order of \(y\). Note that it's only fast when the \(|S|\) is very small.
--
-- @since 1.1.0.0
{-# INLINE descAssocsIn #-}
descAssocsIn :: WaveletMatrix -> Int -> Int -> [(Int, Int)]
descAssocsIn WaveletMatrix {..} l r = Rwm.descAssocsInWith rawWm l r (yDictWm VG.!)
