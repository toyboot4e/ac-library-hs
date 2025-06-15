{-# LANGUAGE RecordWildCards #-}

-- | Two-dimensional segment tree for commutative monoids in \([0, w) \times [0, h)\).
--
-- ==== __Internals__
-- Take a 2x4 matrix as an example:
--
-- @
-- 5 6 7 8
-- 1 2 3 4
-- @
--
-- Extend each row as a segment tree:
--
-- @
--  - 22 11 15  5  6  7  8
--  - 10  3  7  1  2  3  4
-- @
--
-- Then extend each column as a segment tree:
--
-- @
--  -  -  -  -  -  -  -  -
--  - 30 14 22  6  8 10 12
--  - 26 11 15  5  6  7  8
--  - 10  3  7  1  2  3  4
-- @
--
-- ==== __ Example__
-- Create a two-dimensional segment tree for size (w, h) = (4, 2):
--
-- >>> import AtCoder.Extra.SegTree2d.Dense qualified as Seg
-- >>> import Data.Semigroup (Sum (..))
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> seg <- Seg.build @_ @(Sum Int) 4 2 $ VU.fromList [0, 1, 2, 3, 4, 5, 6, 7]
--
-- Get monoid product in \([x_1, x_2) \times [y_1, y_2)\) with `prod`:
--
-- >>> Seg.prod seg {- x -} 1 4 {- y -} 0 1
-- Sum {getSum = 6}
--
-- Monoid values can be altered:
--
-- >>> Seg.write seg 1 1 20
-- >>> Seg.prod seg {- x -} 0 2 {- y -} 0 2
-- Sum {getSum = 25}
--
-- >>> Seg.allProd seg
-- Sum {getSum = 43}
--
-- @since 1.2.3.0
module AtCoder.Extra.SegTree2d.Dense
  ( -- * DenseSegTree2d
    DenseSegTree2d (..),

    -- * Constructors
    new,
    build,
    build',

    -- * Read
    read,
    readMaybe,

    -- * Write
    write,
    modify,
    modifyM,

    -- * Monoid product
    prod,
    allProd,
  )
where

import AtCoder.Internal.Assert qualified as ACIA
import Control.Monad.Primitive (PrimMonad, PrimState, stToPrim)
import Control.Monad.ST (ST)
import Data.Bits
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import Data.Vector qualified as V
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Stack (HasCallStack)
import Prelude hiding (read)

-- | Two-dimensional segment tree.
--
-- @since 1.2.3.0
data DenseSegTree2d s a = DenseSegTree2d
  { -- | Height
    --
    -- @since 1.2.3.0
    hDst :: {-# UNPACK #-} !Int,
    -- | Width
    --
    -- @since 1.2.3.0
    wDst :: {-# UNPACK #-} !Int,
    -- | Monoid values
    --
    -- @since 1.2.3.0
    dataDst :: !(VUM.MVector s a)
  }

-- | \(O(hw)\) Creates a `DenseSegTree2d` for \([0, w) \times [0, h)\) from \(w\) and \(h\).
--
-- @since 1.2.3.0
{-# INLINEABLE new #-}
new ::
  (PrimMonad m, Monoid a, VU.Unbox a) =>
  -- | Width
  Int ->
  -- | Height
  Int ->
  -- | Dense, two-dimensional segment tree
  m (DenseSegTree2d (PrimState m) a)
new wDst hDst = stToPrim $ do
  dataDst <- VUM.replicate (4 * wDst * hDst) mempty
  pure DenseSegTree2d {..}

-- | \(O(hw)\) Creates a `DenseSegTree2d` from width, height and one-dimensional vector of
-- monoid values.
--
-- @since 1.2.3.0
{-# INLINE build #-}
build ::
  (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) =>
  -- | Width
  Int ->
  -- | Height
  Int ->
  -- | Vector of monoid values
  VU.Vector a ->
  -- | Dense, two-dimensional segment tree
  m (DenseSegTree2d (PrimState m) a)
build w h xs = stToPrim $ buildST $ V.unfoldrExactN h (VU.splitAt w) xs
  where
    !_ = ACIA.runtimeAssert (VU.length xs == w * h) "AtCoder.Extra.SegTree2d.Dense.build: vector length mismatch"

-- | \(O(hw)\) Creates a `DenseSegTree2d` from a two-dimensional vector of monoid values.
-- The vector must be indexed by \(y\) first then \(x\): @vec V.! y VU.! x@.
--
-- ==== Constraints
-- - The length of the monoid value vector must be \(hw\).
--
-- @since 1.2.3.0
{-# INLINE build' #-}
build' ::
  (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) =>
  -- | Two-dimensional vector of monoid values
  V.Vector (VU.Vector a) ->
  -- | Dense, two-dimensional segment tree
  m (DenseSegTree2d (PrimState m) a)
build' xs = stToPrim $ buildST xs

-- | \(O(1)\) Returns the monoid value at \((x, y)\).
--
-- @since 1.2.3.0
{-# INLINE read #-}
read :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => DenseSegTree2d (PrimState m) a -> Int -> Int -> m a
read DenseSegTree2d {..} x y = do
  let !_ = ACIA.checkPoint2d "AtCoder.Extra.SegTree2d.Dense.read" x y wDst hDst
  VGM.read dataDst $ idx wDst (y + hDst) (x + wDst)

-- | \(O(1)\) Returns the monoid value at \((x, y)\), or `Nothing` if the point is out of the
-- bounds.
--
-- @since 1.2.3.0
{-# INLINE readMaybe #-}
readMaybe :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => DenseSegTree2d (PrimState m) a -> Int -> Int -> m (Maybe a)
readMaybe DenseSegTree2d {..} x y
  | ACIA.testPoint2d x y wDst hDst = do
      Just <$> VGM.read dataDst (idx wDst (y + hDst) (x + wDst))
  | otherwise = pure Nothing

-- | \(O(\log  h \log w)\) Writes to the \(k\)-th original point's monoid value.
--
-- @since 1.2.3.0
{-# INLINE write #-}
write :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => DenseSegTree2d (PrimState m) a -> Int -> Int -> a -> m ()
write seg x y a = stToPrim $ do
  modifyM seg (pure . const a) x y

-- | \(O(\log  h \log w)\) Given a user function \(f\), modifies the monoid value at \((x, y)\) with
-- it.
--
-- @since 1.2.3.0
{-# INLINE modify #-}
modify :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => DenseSegTree2d (PrimState m) a -> (a -> a) -> Int -> Int -> m ()
modify seg f x y = stToPrim $ do
  modifyM seg (pure . f) x y

-- | \(O(\log h \log w)\) Given a user function \(f\), modifies the monoid value at \((x, y)\) with
-- it.
--
-- @since 1.2.3.0
{-# INLINEABLE modifyM #-}
modifyM :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => DenseSegTree2d (PrimState m) a -> (a -> m a) -> Int -> Int -> m ()
modifyM DenseSegTree2d {..} f x0_ y0_ = do
  let !_ = ACIA.checkPoint2d "AtCoder.Extra.SegTree2d.Dense.modifyM" x0_ y0_ wDst hDst
  let y0 = y0_ + hDst
  let x0 = x0_ + wDst
  VGM.modifyM dataDst f (idx wDst y0 x0)
  stToPrim $ do
    -- right to left
    let updateCurrentRow 0 = pure ()
        updateCurrentRow x = do
          xl <- VGM.read dataDst (idx wDst y0 (2 * x + 0))
          xr <- VGM.read dataDst (idx wDst y0 (2 * x + 1))
          VGM.write dataDst (idx wDst y0 x) $! xl <> xr
          updateCurrentRow (x `div` 2)
    updateCurrentRow (x0 `div` 2)

    -- down to up
    let updateOtherRow 0 = pure ()
        updateOtherRow y = do
          let updateRow 0 = pure ()
              updateRow x = do
                xl <- VGM.read dataDst (idx wDst (2 * y + 0) x)
                xr <- VGM.read dataDst (idx wDst (2 * y + 1) x)
                VGM.write dataDst (idx wDst y x) $! xl <> xr
                updateRow (x `div` 2)
          updateRow x0
          updateOtherRow (y `div` 2)
    updateOtherRow (y0 `div` 2)

-- | \(O(\log h \log w)\) Returns monoid product \(\Pi_{p \in [x_1, x_2) \times [y_1, y_2)} a_p\).
--
-- @since 1.2.3.0
{-# INLINE prod #-}
prod :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => DenseSegTree2d (PrimState m) a -> Int -> Int -> Int -> Int -> m a
prod seg@DenseSegTree2d {..} x1 x2 y1 y2 = stToPrim $ do
  let !_ = ACIA.checkRectShape "AtCoder.Extra.SegTree2d.Dense.prodST" x1 x2 y1 y2
  prodST seg (max 0 x1) (min wDst x2) (max 0 y1) (min hDst y2)

-- | \(O(1)\) Returns monoid product \(\Pi_{p \in [0, w) \times [0, h)} a_p\).
--
-- @since 1.2.3.0
{-# INLINE allProd #-}
allProd :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => DenseSegTree2d (PrimState m) a -> m a
allProd DenseSegTree2d {..} = stToPrim $ do
  fromMaybe mempty <$> VGM.readMaybe dataDst (idx wDst 1 1)

-- -------------------------------------------------------------------------------------------------
-- Internal
-- -------------------------------------------------------------------------------------------------

{-# INLINE idx #-}
idx :: Int -> Int -> Int -> Int
idx w y x = y * (2 * w) + x

{-# INLINEABLE buildST #-}
buildST :: (HasCallStack, Monoid a, VU.Unbox a) => V.Vector (VU.Vector a) -> ST s (DenseSegTree2d s a)
buildST vec = do
  let hDst = V.length vec
  let wDst = VU.length (V.head vec)

  -- NOTE: It's zero-based and we do not ceil H/W to 2^n, still the indexing works fine:
  --           1  2  3
  -- 11  6  5  1  2  3
  dataDst <- VUM.replicate (4 * hDst * wDst) mempty

  -- copy the base matrix in [w, 2w) \times [h, 2h):
  V.iforM_ vec $ \y vs -> do
    VU.iforM_ vs $ \x v -> do
      VGM.write dataDst (idx wDst (hDst + y) (wDst + x)) v

  -- extend the row (y >= h) as a segment tree's internal vector:
  for_ [hDst .. 2 * hDst - 1] $ \y -> do
    for_ [wDst - 1, wDst - 2 .. 0] $ \x -> do
      xl <- VGM.read dataDst (idx wDst y (2 * x + 0))
      xr <- VGM.read dataDst (idx wDst y (2 * x + 1))
      VGM.write dataDst (idx wDst y x) $! xl <> xr

  -- extend each column as a segment tree:
  -- NOTE (pref): interate from y then x for contiguous memory access
  for_ [hDst - 1, hDst - 2 .. 0] $ \y -> do
    for_ [0 .. 2 * wDst - 1] $ \x -> do
      xl <- VGM.read dataDst (idx wDst (2 * y + 0) x)
      xr <- VGM.read dataDst (idx wDst (2 * y + 1) x)
      VGM.write dataDst (idx wDst y x) $! xl <> xr

  pure DenseSegTree2d {..}

{-# INLINEABLE prodST #-}
prodST :: (HasCallStack, Monoid a, VU.Unbox a) => DenseSegTree2d s a -> Int -> Int -> Int -> Int -> ST s a
prodST seg@DenseSegTree2d {..} x1 x2 y1 y2 = do
  inner mempty (y1 + hDst) (y2 + hDst - 1)
  where
    -- inclusive interval [yl, yr]
    inner !acc yl yr
      | yl > yr = pure acc
      | otherwise = do
          acc' <-
            if testBit yl 0
              then (acc <>) <$> prodX seg yl x1 x2
              else pure acc
          acc'' <-
            if not $ testBit yr 0
              then (<> acc') <$> prodX seg yr x1 x2
              else pure acc'
          inner acc'' ((yl + 1) .>>. 1) ((yr - 1) .>>. 1)

{-# INLINEABLE prodX #-}
prodX :: (HasCallStack, Monoid a, VU.Unbox a) => DenseSegTree2d s a -> Int -> Int -> Int -> ST s a
prodX DenseSegTree2d {..} y x1 x2 = do
  inner mempty (x1 + wDst) (x2 + wDst - 1)
  where
    -- inclusive interval [xl, xr]
    inner !acc xl xr
      | xl > xr = pure acc
      | otherwise =do
          acc' <-
            if testBit xl 0
              then (acc <>) <$> VGM.read dataDst (idx wDst y xl)
              else pure acc
          acc'' <-
            if not $ testBit xr 0
              then (<> acc') <$> VGM.read dataDst (idx wDst y xr)
              else pure acc'
          inner acc'' ((xl + 1) .>>. 1) ((xr - 1) .>>. 1)
