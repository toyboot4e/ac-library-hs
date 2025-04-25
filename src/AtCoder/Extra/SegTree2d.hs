{-# LANGUAGE RecordWildCards #-}

-- | Two-dimensional segment tree for commutative monoids at fixed points.
--
-- ==== `SegTree2d` vs `WaveletMatrix2d`
-- They basically have the same functionalities and performance, however, `SegTree2d` performs better in
-- @ac-library-hs@.
--
-- ==== __Examples__
-- Create a two-dimensional segment tree for points \((0, 0)\) with weight \(10\) and \((1, 1)\)
-- with weight \(20\):
--
-- >>> import AtCoder.Extra.SegTree2d qualified as Seg
-- >>> import Data.Semigroup (Sum (..))
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> seg <- Seg.build3 @_ @(Sum Int) $ VU.fromList [(0, 0, 10), (1, 1, 20)]
--
-- Get monoid product in \([x_1, x_2) \times [y_1, y_2)\) with `prod`:
--
-- >>> Seg.prod seg {- x -} 0 2 {- y -} 0 2
-- Sum {getSum = 30}
--
-- Monoid values can be altered:
--
-- >>> Seg.write seg 1 50
-- >>> Seg.prod seg {- x -} 1 2 {- y -} 1 2
-- Sum {getSum = 50}
--
-- >>> Seg.allProd seg
-- Sum {getSum = 60}
--
-- >>> Seg.count seg {- x -} 0 2 {- y -} 0 2
-- 2
--
-- @since 1.2.3.0
module AtCoder.Extra.SegTree2d
  ( -- * SegTree2d
    SegTree2d (..),

    -- * Constructors
    new,
    build,
    build2,
    build3,

    -- * Write

    -- read,
    write,
    modify,
    modifyM,

    -- * Monoid product
    prod,
    allProd,

    -- * Count
    count,
  )
where

import AtCoder.Extra.Bisect (lowerBound)
import AtCoder.Extra.Vector (argsort)
import AtCoder.Internal.Assert qualified as ACIA
import AtCoder.Internal.Bit qualified as ACIB
import Control.Monad (when)
import Control.Monad.Primitive (PrimMonad, PrimState, stToPrim)
import Control.Monad.ST (ST)
import Data.Bits
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import Data.Vector.Algorithms.Intro qualified as VAI
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Stack (HasCallStack)
import Prelude hiding (read)

-- | Two-dimensional segment tree.
--
-- @since 1.2.3.0
data SegTree2d s a = SegTree2d
  { -- | The number of nodes.
    --
    -- @since 1.2.3.0
    nSt :: {-# UNPACK #-} !Int,
    -- | The number of distinct \(x\) coordinates.
    --
    -- @since 1.2.3.0
    nxSt :: {-# UNPACK #-} !Int,
    -- | \(\lceil \log_2 (\mathrm{nx} + 1) \rceil\)
    --
    -- @since 1.2.3.0
    logSt :: {-# UNPACK #-} !Int,
    -- | \(2^{\mathrm{logSt}}\)
    --
    -- @since 1.2.3.0
    sizeSt :: {-# UNPACK #-} !Int,
    -- | \(x\) coordinates sorted and uniquified
    --
    -- @since 1.2.3.0
    dictXSt :: {-# UNPACK #-} !(VU.Vector Int),
    -- | \(y\) coordinates sorted (not uniquified)
    --
    -- @since 1.2.3.0
    allYSt :: !(VU.Vector Int),
    -- |
    --
    -- @since 1.2.3.0
    posSt :: !(VU.Vector Int),
    -- |
    --
    -- @since 1.2.3.0
    indptrSt :: !(VU.Vector Int),
    -- | Monoid values
    --
    -- @since 1.2.3.0
    dataSt :: !(VUM.MVector s a),
    -- |
    --
    -- @since 1.2.3.0
    toLeftSt :: !(VU.Vector Int)
  }

-- | \(O(n \log n)\) Creates a `SegTree2d` from a vector of \((x, y)\) coordinates.
--
-- @since 1.2.3.0
{-# INLINEABLE new #-}
new ::
  (PrimMonad m, Monoid a, VU.Unbox a) =>
  -- | \((x, y)\) vector
  VU.Vector (Int, Int) ->
  -- | Two-dimensional segment tree
  m (SegTree2d (PrimState m) a)
new xys = stToPrim $ buildST xs ys (VU.replicate n mempty)
  where
    n = VU.length xys
    (!xs, !ys) = VU.unzip xys

-- | \(O(n \log n)\) Creates a `SegTree2d` from vectors of \(x\), \(y\) and \(w\).
--
-- @since 1.2.3.0
{-# INLINE build #-}
build ::
  (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) =>
  -- | \(x\) vector
  VU.Vector Int ->
  -- | \(y\) vector
  VU.Vector Int ->
  -- | \(w\) vector
  VU.Vector a ->
  -- | Two-dimensional segment tree
  m (SegTree2d (PrimState m) a)
build xs ys ws = stToPrim $ buildST xs ys ws

-- | \(O(n \log n)\) Creates a `SegTree2d` from vectors of \((x, y)\) and \(w\).
--
-- @since 1.2.3.0
{-# INLINE build2 #-}
build2 ::
  (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) =>
  -- | \((x, y)\) vector
  VU.Vector (Int, Int) ->
  -- | \(w\) vector
  VU.Vector a ->
  -- | Two-dimensional segment tree
  m (SegTree2d (PrimState m) a)
build2 xys ws = stToPrim $ do
  let (!xs, !ys) = VU.unzip xys
  buildST xs ys ws

-- | \(O(n \log n)\) Creates a `SegTree2d` from a vector of \((x, y, w)\).
--
-- @since 1.2.3.0
{-# INLINE build3 #-}
build3 :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => VU.Vector (Int, Int, a) -> m (SegTree2d (PrimState m) a)
build3 xyws = stToPrim $ do
  let (!xs, !ys, !ws) = VU.unzip3 xyws
  buildST xs ys ws

-- -- | \(O(\log n)\) Read the \(k\)-th original point's monoid value.
-- --
-- -- @since 1.2.3.0
-- {-# INLINE read #-}
-- read :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => SegTree2d (PrimState m) a -> Int -> m ()
-- read SegTree2d {..} i = do
--   let !_ = ACIA.checkIndex "AtCoder.Extra.SegTree2d.read" i nSt
--   -- FIXME: pos is incorrect
--   VGM.read dataSt (posSt VG.! i)

-- | \(O(\log n)\) Writes to the \(k\)-th original point's monoid value.
--
-- @since 1.2.3.0
{-# INLINE write #-}
write ::
  (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) =>
  -- | Two-dimensional segment tree.
  SegTree2d (PrimState m) a ->
  -- | Original point index.
  Int ->
  -- | New monoid value.
  a ->
  m ()
write seg i x = stToPrim $ do
  modifyM seg (pure . const x) i

-- | \(O(\log n)\) Given a user function \(f\), modifies the \(k\)-th original point's monoid value
-- with it.
--
-- @since 1.2.3.0
{-# INLINE modify #-}
modify ::
  (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) =>
  -- | Two-dimensional segment tree.
  SegTree2d (PrimState m) a ->
  -- | Function that alters the monoid value.
  (a -> a) ->
  -- | Original point index.
  Int ->
  m ()
modify seg f i = stToPrim $ do
  modifyM seg (pure . f) i

-- | \(O(\log n)\) Given a user function \(f\), modifies the \(k\)-th original point's monoid value
-- with it.
--
-- @since 1.2.3.0
{-# INLINEABLE modifyM #-}
modifyM ::
  (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) =>
  -- | Two-dimensional segment tree.
  SegTree2d (PrimState m) a ->
  -- | Function that alters the monoid value.
  (a -> m a) ->
  -- | Original point index.
  Int ->
  m ()
modifyM seg@SegTree2d {..} f rawIdx = do
  let !_ = ACIA.checkIndex "AtCoder.Extra.SegTree2d.modifyM" rawIdx nSt
  inner 1 $ posSt VG.! rawIdx
  where
    inner i p = do
      let indptrI = indptrSt VG.! i
      modifyIST seg f i $ p - indptrI
      when (i < sizeSt) $ do
        let lc = toLeftSt VG.! p - toLeftSt VG.! indptrI
        let rc = (p - indptrI) - lc
        if toLeftSt VG.! (p + 1) - toLeftSt VG.! p /= 0
          then do
            let i' = 2 * i + 0
            let p' = indptrSt VG.! i' + lc
            inner i' p'
          else do
            let i' = 2 * i + 1
            let p' = indptrSt VG.! i' + rc
            inner i' p'

-- | \(O(\log^2 n)\) Returns monoid product \(\Pi_{p \in [x_1, x_2) \times [y_1, y_2)} a_p\).
--
-- @since 1.2.3.0
{-# INLINE prod #-}
prod ::
  (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) =>
  -- | Two-dimensional segment tree.
  SegTree2d (PrimState m) a ->
  -- | \(x_1\)
  Int ->
  -- | \(x_2\)
  Int ->
  -- | \(y_1\)
  Int ->
  -- | \(y_2\)
  Int ->
  -- | \(\Pi_{p \in [x_1, x_2) \times [y_1, y_2)} a_p\)
  m a
prod seg lx rx ly ry = stToPrim $ prodST seg lx rx ly ry

-- | \(O(1)\) Returns monoid product \(\Pi_{p \in [-\infty, \infty) \times [-\infty, \infty)} a_p\).
--
-- @since 1.2.3.0
{-# INLINE allProd #-}
allProd :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => SegTree2d (PrimState m) a -> m a
allProd seg = fromMaybe mempty <$> VGM.readMaybe (dataSt seg) 1

-- | \(O(\log n)\) Returns the number of points in \([x_1, x_2) \times [y_1, y_2)\).
--
-- @since 1.2.3.0
{-# INLINE count #-}
count ::
  (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) =>
  -- | Two-dimensional segment tree.
  SegTree2d (PrimState m) a ->
  -- | \(x_1\)
  Int ->
  -- | \(x_2\)
  Int ->
  -- | \(y_1\)
  Int ->
  -- | \(y_2\)
  Int ->
  -- | The number of points in \([x_1, x_2) \times [y_1, y_2)\).
  m Int
count seg lx rx ly ry = stToPrim $ countST seg lx rx ly ry

-- -------------------------------------------------------------------------------------------------
-- Internal
-- -------------------------------------------------------------------------------------------------

{-# INLINEABLE buildST #-}
buildST :: forall s a. (HasCallStack, Monoid a, VU.Unbox a) => VU.Vector Int -> VU.Vector Int -> VU.Vector a -> ST s (SegTree2d s a)
buildST xs ys ws = do
  let nSt = VU.length xs
  let !_ = ACIA.runtimeAssert (nSt == VU.length ys && nSt == VU.length ws) "AtCoder.Extra.SegTree2d.buildST: length mismatch among `xs`, `ys` and `ws`"

  -- TODO: use radix sort?
  -- we don't have to take `uniq` though:
  let dictXSt = VU.uniq $ VU.modify VAI.sort xs
  let nxSt = VU.length dictXSt
  let logSt = countTrailingZeros $ ACIB.bitCeil (nxSt + 1)
  let sizeSt = bit logSt
  let compressedXs = VU.map (lowerBound dictXSt) xs

  -- TODO: what is this?
  let indptrSt = VU.create $ do
        indptr <- VUM.replicate (2 * sizeSt + 1) (0 :: Int)
        VU.forM_ compressedXs $ \i -> do
          let inner j
                | j /= 0 = do
                    VGM.modify indptr (+ 1) (j + 1) -- +1 for perfix sum
                    inner (j `div` 2)
                | otherwise = pure ()
          inner $ i + sizeSt
        -- calculate prefix sum in-place:
        VUM.iforM_ (VUM.init indptr) $ \i x -> do
          VGM.modify indptr (+ x) (i + 1)
        pure indptr

  dataSt <- VUM.replicate (2 * VU.last indptrSt) (mempty :: a)

  let yis = argsort ys
  let posSt = VU.create $ do
        vec <- VUM.replicate nSt (0 :: Int)
        VU.iforM_ yis $ \i yi -> do
          -- FIXME: It overwrites duplicate yi to i. Isn't it just `accumulate max`?
          VGM.write vec yi i
        pure vec

  -- +1 for prefix cumulative sum
  toLeftSt <- do
    toLeft <- VUM.replicate (indptrSt VG.! sizeSt + 1) (0 :: Int)
    ptr <- VU.thaw indptrSt
    VU.forM_ yis $ \rawIdx -> do
      let inner i j
            | i > 0 = do
                p <- VGM.read ptr i
                VGM.write ptr i $ p + 1
                VGM.write dataSt (indptrSt VG.! (i + 1) + p) $ ws VG.! rawIdx
                when (j /= -1) $ do
                  VGM.write toLeft (p + 1) $ if even j then 1 else 0
                inner (i `div` 2) i
            | otherwise = pure ()
      let i0 = compressedXs VG.! rawIdx + sizeSt
      inner i0 (-1)

    -- calculate prefix cumulative sum in-place:
    VUM.iforM_ (VUM.init toLeft) $ \i x -> do
      VGM.modify toLeft (+ x) (i + 1)
    VU.unsafeFreeze toLeft

  for_ [0 .. 2 * sizeSt - 1] $ \i -> do
    let off = 2 * indptrSt VG.! i
    let n = indptrSt VG.! (i + 1) - indptrSt VG.! i
    for_ [n - 1, n - 2 .. 1] $ \j -> do
      xl <- VGM.read dataSt $ off + 2 * j + 0
      xr <- VGM.read dataSt $ off + 2 * j + 1
      VGM.write dataSt (off + j) $! xl <> xr

  -- TODO: why not uniquified?
  let allYSt = VU.modify VAI.sort ys
  pure SegTree2d {..}

{-# INLINEABLE modifyIST #-}
modifyIST :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => SegTree2d (PrimState m) a -> (a -> m a) -> Int -> Int -> m ()
modifyIST SegTree2d {..} f i j0 = do
  -- TODO: LID?
  let lid = indptrSt VG.! i
  let off = 2 * lid
  let inner j_ = do
        when (j_ > 1) $ do
          let j = j_ `div` 2
          xl <- VGM.read dataSt (off + 2 * j + 0)
          xr <- VGM.read dataSt (off + 2 * j + 1)
          VGM.write dataSt (off + j) $! xl <> xr
          inner j
  let j = j0 + indptrSt VG.! (i + 1) - lid
  VGM.modifyM dataSt f (off + j)
  stToPrim $ inner j

{-# INLINEABLE prodST #-}
prodST :: forall s a. (HasCallStack, Monoid a, VU.Unbox a) => SegTree2d s a -> Int -> Int -> Int -> Int -> ST s a
prodST seg@SegTree2d {..} lx rx ly ry = do
  let a0 = lowerBound allYSt ly
  let b0 = lowerBound allYSt ry
  dfs mempty 1 0 sizeSt a0 b0
  where
    !_ = ACIA.runtimeAssert (lx <= rx && ly <= ry) "AtCoder.Extra.SegTree2d.prodST: given invalid rectangle"
    !l0 = lowerBound dictXSt lx
    !r0 = lowerBound dictXSt rx
    dfs :: a -> Int -> Int -> Int -> Int -> Int -> ST s a
    dfs !res i l r a b
      -- empty rect
      | a == b = pure res
      -- not intersecting
      | r0 <= l || r <= l0 = pure res
      -- fully contained in [l_0, r_0)
      | l0 <= l && r <= r0 = do
          xi <- prodIST seg i a b
          pure $! xi <> res
      | otherwise = do
          let indptrI = indptrSt VG.! i
          let toLeftI = toLeftSt VG.! indptrI
          let la = toLeftSt VG.! (indptrI + a) - toLeftI
          let ra = a - la
          let lb = toLeftSt VG.! (indptrI + b) - toLeftI
          let rb = b - lb
          let m = (l + r) `div` 2
          res' <- dfs res (2 * i + 0) l m la lb
          dfs res' (2 * i + 1) m r ra rb

{-# INLINEABLE prodIST #-}
prodIST :: (HasCallStack, Monoid a, VU.Unbox a) => SegTree2d s a -> Int -> Int -> Int -> ST s a
prodIST SegTree2d {..} i a b = inner mempty (n + a) (n + b - 1)
  where
    lid = indptrSt VG.! i
    off = 2 * lid
    n = indptrSt VG.! (i + 1) - lid
    -- NOTE: we're using inclusive interval [l, r] for simplicity
    inner !res l r
      | l <= r = do
          res' <-
            if testBit l 0
              then (res <>) <$> VGM.read dataSt (off + l)
              else pure res
          res'' <-
            if not $ testBit r 0
              then (<> res') <$> VGM.read dataSt (off + r)
              else pure res'
          inner res'' ((l + 1) .>>. 1) ((r - 1) .>>. 1)
      | otherwise = pure res

{-# INLINEABLE countST #-}
countST :: forall s a. (HasCallStack, Monoid a, VU.Unbox a) => SegTree2d s a -> Int -> Int -> Int -> Int -> ST s Int
countST SegTree2d {..} lx rx ly ry = do
  let a0 = lowerBound allYSt ly
  let b0 = lowerBound allYSt ry
  dfs 0 1 0 sizeSt a0 b0
  where
    !_ = ACIA.runtimeAssert (lx <= rx && ly <= ry) "AtCoder.Extra.SegTree2d.countST: given invalid rectangle"
    !l0 = lowerBound dictXSt lx
    !r0 = lowerBound dictXSt rx
    dfs :: Int -> Int -> Int -> Int -> Int -> Int -> ST s Int
    dfs (res :: Int) i l r a b
      -- empty rect
      | a == b = pure res
      -- not intersecting
      | r0 <= l || r <= l0 = pure res
      -- fully contained in [l_0, r_0)
      | l0 <= l && r <= r0 = do
          pure $! res + b - a
      | otherwise = do
          let indptrI = indptrSt VG.! i
          let toLeftI = toLeftSt VG.! indptrI
          let la = toLeftSt VG.! (indptrI + a) - toLeftI
          let ra = a - la
          let lb = toLeftSt VG.! (indptrI + b) - toLeftI
          let rb = b - lb
          let m = (l + r) `div` 2
          res' <- dfs res (2 * i + 0) l m la lb
          dfs res' (2 * i + 1) m r ra rb
