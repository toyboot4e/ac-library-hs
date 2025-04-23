{-# LANGUAGE RecordWildCards #-}

-- | A 2D, static wavelet matrix with segment tree, that can handle point add and rectangle sum
-- queries. Points cannot be added after construction, but monoid values in each point can be
-- modified later.
--
-- ==== `SegTree2d` vs `WaveletMatrix2d`
-- They basically have the same functionalities and performance, however, `SegTree2d` performs better in
-- @ac-library-hs@.
--
-- ==== __Example__
-- Create a `WaveletMatrix2d` with initial vertex values:
--
-- >>> import AtCoder.Extra.WaveletMatrix2d qualified as WM
-- >>> import Data.Semigroup (Sum (..))
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> -- 8  9 10 11
-- >>> -- 4  5  6  7
-- >>> -- 0  1  2  3
-- >>> wm <- WM.build negate $ VU.generate 12 $ \i -> let (!y, !x) = i `divMod` 4 in (x, y, Sum i)
--
-- Read the value at \(x = 2, y = 1\):
--
-- >>> WM.read wm (2, 1)
-- Sum {getSum = 6}
--
-- Other segment tree methods are also available, but in 2D:
--
-- >>> WM.allProd wm -- (0 + 11) * 12 / 2 = 66
-- Sum {getSum = 66}
--
-- >>> WM.prod wm {- x -} 1 3 {- y -} 0 3 -- 1 + 2 + 5 + 6 + 9 + 10
-- Sum {getSum = 33}
--
-- >>> WM.modify wm (+ 2) (1, 1)
-- >>> WM.prod wm {- x -} 1 3 {- y -} 0 3 -- 1 + 2 + 7 + 6 + 9 + 10
-- Sum {getSum = 35}
--
-- >>> WM.write wm (1, 1) $ Sum 0
-- >>> WM.prod wm {- x -} 1 3 {- y -} 0 3 -- 1 + 2 + 0 + 6 + 9 + 10
-- Sum {getSum = 28}
--
-- @since 1.1.0.0
module AtCoder.Extra.WaveletMatrix2d
  ( -- * Wavelet matrix 2D
    WaveletMatrix2d (..),

    -- * Counstructor
    new,
    build,

    -- * Segment tree methods
    read,
    write,
    modify,
    prod,
    prodMaybe,
    allProd, -- FIXME: rename to prodAll
    -- wavelet matrix methods could be implemented, too
  )
where

import AtCoder.Extra.Bisect (lowerBound)
import AtCoder.Extra.WaveletMatrix.BitVector qualified as BV
import AtCoder.Extra.WaveletMatrix.Raw qualified as Rwm
import AtCoder.Internal.Assert qualified as ACIA
import AtCoder.SegTree qualified as ST
import Control.Monad.Primitive (PrimMonad, PrimState, stToPrim)
import Control.Monad.ST (ST)
import Data.Bit (Bit (..))
import Data.Bits (Bits (testBit))
import Data.Vector qualified as V
import Data.Vector.Algorithms.Intro qualified as VAI
import Data.Vector.Generic qualified as VG
import Data.Vector.Unboxed qualified as VU
import GHC.Stack (HasCallStack)
import Prelude hiding (read)

-- NOTE: There are many possible improvements.
-- - Use cumulative sum or fenwick tree instead for the speed.
-- - The inverse operator is not actually required.
-- - Wavelet matrix methods such as `rank` can be implemented
-- - `maxRight` can be implemented.

-- | Segment Tree on Wavelet Matrix: points on a 2D plane and rectangle products.
--
-- @since 1.3.0.0
data WaveletMatrix2d s a = WaveletMatrix2d
  { -- | The wavelet matrix that represents points on a 2D plane.
    --
    -- @since 1.3.0.0
    rawWm2d :: !Rwm.RawWaveletMatrix,
    -- | (x, y) index compression dictionary.
    --
    -- @since 1.1.0.0
    xyDictWm2d :: !(VU.Vector (Int, Int)),
    -- | y index compression dictionary.
    --
    -- @since 1.1.0.0
    yDictWm2d :: !(VU.Vector Int),
    -- | The segment tree of the weights of the points in the order of `xyDictWm2d`.
    --
    -- @since 1.1.0.0
    segTreesWm2d :: !(V.Vector (ST.SegTree s a)),
    -- | The inverse operator of the interested monoid.
    --
    -- @since 1.1.0.0
    invWm2d :: !(a -> a)
  }

-- | \(O(n \log n)\) Creates a `WaveletMatrix2d` with `mempty` as the initial monoid
-- values for each point.
--
-- @since 1.1.0.0
{-# INLINEABLE new #-}
new ::
  (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) =>
  -- | Inverse operator of the monoid
  (a -> a) ->
  -- | Input points
  VU.Vector (Int, Int) ->
  -- | A 2D wavelet matrix
  m (WaveletMatrix2d (PrimState m) a)
new invWm2d xys = stToPrim $ do
  let n = VG.length xys
  let xyDictWm2d = VU.uniq . VU.modify (VAI.sortBy compare) $ xys
  let (!_, !ys) = VU.unzip xys
  let yDictWm2d = VU.uniq $ VU.modify (VAI.sortBy compare) ys
  -- REMARK: Be sure to use `n + 1` because the product function cannot handle the case
  --         `yUpper` is `2^{height}`.
  let (!_, !ysInput) = VU.unzip xyDictWm2d
  let rawWm2d = Rwm.build (n + 1) $ VU.map (lowerBound yDictWm2d) ysInput
  segTreesWm2d <- V.replicateM (Rwm.heightRwm rawWm2d) (ST.new n)
  pure WaveletMatrix2d {..}

-- | \(O(n \log n)\) Creates a `WaveletMatrix2d` with wavelet matrix with segment tree
-- with initial monoid values. Monoids on a duplicate point are accumulated with `(<>)`.
--
-- @since 1.1.0.0
{-# INLINEABLE build #-}
build ::
  (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) =>
  -- | Inverse operator of the monoid
  (a -> a) ->
  -- | Input points with initial values
  VU.Vector (Int, Int, a) ->
  -- | A 2D wavelet matrix
  m (WaveletMatrix2d (PrimState m) a)
build invWm2d xysw = stToPrim $ do
  let (!xs, !ys, !_) = VU.unzip3 xysw
  wm <- new invWm2d $ VU.zip xs ys
  -- not the fastest implementation though
  VU.forM_ xysw $ \(!x, !y, !w) -> do
    modify wm (<> w) (x, y)
  pure wm

-- | \(O(1)\) Returns the monoid value at \((x, y)\).
--
-- @since 1.1.0.0
{-# INLINEABLE read #-}
read :: (HasCallStack, PrimMonad m, VU.Unbox a, Monoid a) => WaveletMatrix2d (PrimState m) a -> (Int, Int) -> m a
read WaveletMatrix2d {..} (!x, !y) = do
  ST.read (V.head segTreesWm2d) $ lowerBound xyDictWm2d (x, y)

-- | \(O(\log^2 n)\) Writes the monoid value at \((x, y)\). Access to unknown points are undefined.
--
-- @since 1.1.0.0
{-# INLINEABLE write #-}
write :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => WaveletMatrix2d (PrimState m) a -> (Int, Int) -> a -> m ()
write WaveletMatrix2d {..} (!x, !y) v = stToPrim $ do
  let !i_ = lowerBound xyDictWm2d (x, y)
  V.ifoldM'_
    ( \i iRow (!bits, !seg) -> do
        let !i0 = BV.rank0 bits i
        let !i'
              | unBit $ VG.unsafeIndex (BV.bitsBv bits) i =
                  i + Rwm.nZerosRwm rawWm2d VG.! iRow - i0
              | otherwise = i0
        ST.write seg i' v
        pure i'
    )
    i_
    $ V.zip (Rwm.bitsRwm rawWm2d) segTreesWm2d

-- | \(O(\log^2 n)\) Modifies the monoid value at \((x, y)\). Access to unknown points are
-- undefined.
--
-- @since 1.1.0.0
{-# INLINEABLE modify #-}
modify :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => WaveletMatrix2d (PrimState m) a -> (a -> a) -> (Int, Int) -> m ()
modify WaveletMatrix2d {..} f (!x, !y) = stToPrim $ do
  let !i_ = lowerBound xyDictWm2d (x, y)
  V.ifoldM'_
    ( \i iRow (!bits, !seg) -> do
        let !i0 = BV.rank0 bits i
        let !i'
              | unBit $ VG.unsafeIndex (BV.bitsBv bits) i =
                  i + Rwm.nZerosRwm rawWm2d VG.! iRow - i0
              | otherwise = i0
        ST.modify seg f i'
        pure i'
    )
    i_
    $ V.zip (Rwm.bitsRwm rawWm2d) segTreesWm2d

-- | \(O(\log^2 n)\) Returns monoid product \(\Pi_{p \in [x_1, x_2) \times [y_1, y_2)} a_p\).
--
-- @since 1.1.0.0
{-# INLINEABLE prod #-}
prod :: (HasCallStack, PrimMonad m, VU.Unbox a, Monoid a) => WaveletMatrix2d (PrimState m) a -> Int -> Int -> Int -> Int -> m a
prod wm@WaveletMatrix2d {..} !xl !xr !yl !yr
  | xl' >= xr' || yl' >= yr' = pure mempty
  | otherwise = unsafeProd wm xl' xr' yl' yr'
  where
    (!xDict, !_) = VU.unzip xyDictWm2d
    -- NOTE: clamping here!
    xl' = lowerBound xDict xl
    xr' = lowerBound xDict xr
    yl' = lowerBound yDictWm2d yl
    yr' = lowerBound yDictWm2d yr
    !_ = ACIA.checkInterval "AtCoder.Extra.WaveletMatrix.SegTree.prod (compressed x)" xl' xr' (VG.length xDict)
    !_ = ACIA.checkInterval "AtCoder.Extra.WaveletMatrix.SegTree.prod (compressed y)" yl' yr' (VG.length yDictWm2d)

-- | \(O(\log^2 n)\) Returns the monoid product in \([x_1, x_2) \times [y_1, y_2)\). Returns `Nothing` for invalid
-- intervals.
--
-- @since 1.1.0.0
{-# INLINEABLE prodMaybe #-}
prodMaybe :: (PrimMonad m, VU.Unbox a, Monoid a) => WaveletMatrix2d (PrimState m) a -> Int -> Int -> Int -> Int -> m (Maybe a)
prodMaybe wm@WaveletMatrix2d {..} !xl !xr !yl !yr
  | not (ACIA.testInterval xl' xr' (VG.length xDict)) = pure Nothing
  | not (ACIA.testInterval yl' yr' (VG.length yDictWm2d)) = pure Nothing
  | xl' >= xr' || yl' >= yr' = pure $ Just mempty
  | otherwise = Just <$> unsafeProd wm xl' xr' yl' yr'
  where
    (!xDict, !_) = VU.unzip xyDictWm2d
    -- NOTE: clamping here!
    xl' = lowerBound xDict xl
    xr' = lowerBound xDict xr
    yl' = lowerBound yDictWm2d yl
    yr' = lowerBound yDictWm2d yr

-- | \(O(\log^2 n)\) Return the monoid product in \([-\infty, \infty) \times [-\infty, \infty)\).
--
-- @since 1.1.0.0
{-# INLINEABLE allProd #-}
allProd :: (HasCallStack, PrimMonad m, PrimMonad m, VU.Unbox a, Monoid a) => WaveletMatrix2d (PrimState m) a -> m a
allProd WaveletMatrix2d {..} = do
  -- ST.allProd (V.last segTreesWm2d)
  ST.allProd (V.head segTreesWm2d)

-- | \(O(\log^2 n)\) The input is compressed indices.
--
-- @since 1.1.0.0
{-# INLINE unsafeProd #-}
unsafeProd :: (PrimMonad m, VU.Unbox a, Monoid a) => WaveletMatrix2d (PrimState m) a -> Int -> Int -> Int -> Int -> m a
unsafeProd wm xl' xr' yl' yr' = stToPrim $ do
  sR <- prodLT wm xl' xr' yr'
  sL <- prodLT wm xl' xr' yl'
  pure $! sR <> invWm2d wm sL

-- | \(O(\log^2 n)\)
{-# INLINEABLE prodLT #-}
prodLT :: (Monoid a, VU.Unbox a) => WaveletMatrix2d s a -> Int -> Int -> Int -> ST s a
prodLT WaveletMatrix2d {..} !l_ !r_ yUpper = do
  (!res, !_, !_) <- do
    V.ifoldM'
      ( \(!acc, !l, !r) !iRow (!bits, !seg) -> do
          let !l0 = BV.rank0 bits l
              !r0 = BV.rank0 bits r
          -- REMARK: The function cannot handle the case yUpper = N = 2^i. See the constructor for
          -- how it's handled and note that l_ and r_ are compressed indices.
          if testBit yUpper (Rwm.heightRwm rawWm2d - 1 - iRow)
            then do
              !acc' <- (acc <>) <$> ST.prod seg l0 r0
              let !l' = l + Rwm.nZerosRwm rawWm2d VG.! iRow - l0
              let !r' = r + Rwm.nZerosRwm rawWm2d VG.! iRow - r0
              pure (acc', l', r')
            else do
              pure (acc, l0, r0)
      )
      (mempty, l_, r_)
      $ V.zip (Rwm.bitsRwm rawWm2d) segTreesWm2d
  pure res
