{-# LANGUAGE RecordWildCards #-}

-- | Static, \(k\)-dimensional tree \((k = 2)\) with lazily propagated monoid actions and
-- commutative monoids.
--
-- - Point coordinates are fixed on `build`.
-- - Multiple points can exist at the same coordinate.
--
-- ==== __Examples__
-- >>> import AtCoder.Extra.LazyKdTree qualified as LKT
-- >>> import AtCoder.Extra.Monoid.Affine1 (Affine1)
-- >>> import AtCoder.Extra.Monoid.Affine1 qualified as Affine1
-- >>> import Data.Semigroup (Sum (..))
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> let xyws = VU.fromList [(0, 0, Sum 1), (1, 1, Sum 2), (4, 2, Sum 3)]
-- >>> lkt <- LKT.build3 @_ @(Affine1 Int) @(Sum Int) xyws
--
-- >>> -- Get monoid product in [0, 2) x [0, 2)
-- >>> LKT.prod lkt 0 2 0 2
-- Sum {getSum = 3}
--
-- >>> LKT.applyIn lkt 0 2 0 2 $ Affine1.new 2 1
-- >>> LKT.prod lkt 0 2 0 2
-- Sum {getSum = 8}
--
-- >>> LKT.write lkt 0 $ Sum 10
-- >>> LKT.prod lkt 0 2 0 2
-- Sum {getSum = 15}
--
-- @since 1.2.2.0
module AtCoder.Extra.LazyKdTree
  ( -- * K-dimensional tree
    LazyKdTree (..),

    -- * Re-exports
    SegAct (..),

    -- * Constructors
    build,
    build2,
    build3,

    -- * Write
    write,
    modify,
    modifyM,

    -- * Monoid products
    prod,
    allProd,

    -- * Apply
    applyIn,
  )
where

import AtCoder.Internal.Assert qualified as ACIA
import AtCoder.Internal.Bit qualified as ACIB
import AtCoder.LazySegTree (SegAct (..))
import Control.Monad (unless, when)
import Control.Monad.Primitive (PrimMonad, PrimState, stToPrim)
import Control.Monad.ST (ST)
import Data.Bits
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Vector.Algorithms.Intro qualified as VAI
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Stack (HasCallStack)

-- | Static, \(k\)-dimensional tree \((k = 2)\) with lazily propagated monoid actions and
-- commutative monoids.
--
-- @since 1.2.2.0
data LazyKdTree s f a = LazyKdTree
  { -- | The number of points in the \(k\)-d tree.
    --
    -- @since 1.2.2.0
    nLkt :: {-# UNPACK #-} !Int,
    -- | \(\lceil \log_2 (n + 1) \rceil\)
    --
    -- @since 1.2.2.0
    logLkt :: {-# UNPACK #-} !Int,
    -- | Rectangle information: inclusive (closed) ranges \([x_1, x_2) \times [y_1, y_2)\).
    --
    -- @since 1.2.2.0
    incRectsLkt :: !(VU.Vector (Int, Int, Int, Int)),
    -- | Rectangle information: monoid values.
    --
    -- @since 1.2.2.0
    dataLkt :: !(VUM.MVector s a),
    -- | Rectangle information: lazily propagated monoid actions for children.
    --
    -- @since 1.2.2.0
    lazyLkt :: !(VUM.MVector s f),
    -- | Rectangle information: the number of vertices in the rectangle.
    --
    -- @since 1.2.2.0
    sizeLkt :: !(VU.Vector Int),
    -- | Maps original vertices into the belonging rectangle index.
    --
    -- @since 1.2.2.0
    posLkt :: !(VU.Vector Int)
  }

-- | \(O(n \log n)\) Creates a `LazyKdTree` from @xs@, @ys@ and @ws@ vectors.
--
-- ==== Constraints
-- - \(|\mathrm{xs}| = |\mathrm{ys}| = |\mathrm{vs}|\).
--
-- @since 1.2.2.0
{-# INLINE build #-}
build ::
  (HasCallStack, PrimMonad m, Monoid f, VU.Unbox f, Semigroup a, VU.Unbox a) =>
  -- | \(x\) coordnates
  VU.Vector Int ->
  -- | \(y\) coordnates
  VU.Vector Int ->
  -- | monoid \(v\)alues
  VU.Vector a ->
  -- | `LazyKdTree`
  m (LazyKdTree (PrimState m) f a)
build xs ys vs = stToPrim $ buildST xs ys vs

-- | \(O(n \log n)\) Creates a `LazyKdTree` from @xys@ and @ws@ vectors.
--
-- ==== Constraints
-- - \(|\mathrm{xys}| = |\mathrm{vs}|\).
--
-- @since 1.2.2.0
{-# INLINE build2 #-}
build2 ::
  (HasCallStack, PrimMonad m, Monoid f, VU.Unbox f, Semigroup a, VU.Unbox a) =>
  -- | \((x, y)\) coordinates
  VU.Vector (Int, Int) ->
  -- | Monoid \(v\)alues
  VU.Vector a ->
  -- | `LazyKdTree`
  m (LazyKdTree (PrimState m) f a)
build2 xys ws = stToPrim $ buildST xs ys ws
  where
    (!xs, !ys) = VU.unzip xys

-- | \(O(n \log n)\) Creates a `LazyKdTree` from a @xyws@ vector.
--
-- @since 1.2.2.0
{-# INLINE build3 #-}
build3 ::
  (HasCallStack, PrimMonad m, Monoid f, VU.Unbox f, Semigroup a, VU.Unbox a) =>
  -- | \((x, y, v)\) tuples
  VU.Vector (Int, Int, a) ->
  -- | `LazyKdTree`
  m (LazyKdTree (PrimState m) f a)
build3 xyws = stToPrim $ buildST xs ys ws
  where
    (!xs, !ys, !ws) = VU.unzip3 xyws

-- | \(O(\log n)\) Writes to the \(k\)-th point's monoid value.
--
-- @since 1.2.2.0
{-# INLINE write #-}
write ::
  (HasCallStack, PrimMonad m, SegAct f a, Eq f, VU.Unbox f, Semigroup a, VU.Unbox a) =>
  -- | `LazyKdTree`
  LazyKdTree (PrimState m) f a ->
  -- | Original vertex index.
  Int ->
  -- | Monoid value
  a ->
  -- | Monadic tuple
  m ()
write kt i x = stToPrim $ modifyM kt (pure . const x) i

-- | \(O(\log n)\) Modifies the \(k\)-th point's monoid value.
--
-- @since 1.2.2.0
{-# INLINE modify #-}
modify ::
  (HasCallStack, PrimMonad m, SegAct f a, Eq f, VU.Unbox f, Semigroup a, VU.Unbox a) =>
  -- | `LazyKdTree`
  LazyKdTree (PrimState m) f a ->
  -- | Creates a new monoid value from the old one.
  (a -> a) ->
  -- | Original vertex index.
  Int ->
  -- | Monadic tuple
  m ()
modify kt f i = stToPrim $ modifyM kt (pure . f) i

-- | \(O(\log n)\) Modifies the \(k\)-th point's monoid value.
--
-- @since 1.2.2.0
{-# INLINEABLE modifyM #-}
modifyM ::
  (HasCallStack, PrimMonad m, SegAct f a, Eq f, VU.Unbox f, Semigroup a, VU.Unbox a) =>
  -- | `LazyKdTree`
  LazyKdTree (PrimState m) f a ->
  -- | Creates a new monoid value from the old one.
  (a -> m a) ->
  -- | Original vertex index.
  Int ->
  -- | Monadic tuple
  m ()
modifyM kt@LazyKdTree {..} f i0 = do
  let i_ = posLkt VG.! i0
  -- propagate lazily propagated monoid actions from the root:
  stToPrim $ for_ [logLkt, logLkt - 1 .. 1] $ \k -> do
    pushST kt (i_ .>>. k)
  VGM.modifyM dataLkt f i_
  -- update parents:
  let inner i
        | i <= 1 = pure ()
        | otherwise = do
            let i' = i `div` 2
            xl <- VGM.read dataLkt (2 * i' + 0)
            xr <- VGM.read dataLkt (2 * i' + 1)
            VGM.write dataLkt i' $! xl <> xr
            inner i'
  stToPrim $ inner i_

-- | \(O(\log n)\) Returns monoid product in \([x_l, x_r) \times [y_l, y_r)\).
--
-- @since 1.2.2.0
{-# INLINE prod #-}
prod ::
  (HasCallStack, PrimMonad m, Eq f, SegAct f a, Eq f, VU.Unbox f, Monoid a, VU.Unbox a) =>
  -- | `LazyKdTree`
  LazyKdTree (PrimState m) f a ->
  -- | \(x_l\)
  Int ->
  -- | \(x_r\)
  Int ->
  -- | \(y_l\)
  Int ->
  -- | \(y_r\)
  Int ->
  -- | Monoid product in \([x_l, x_r) \times [y_l, y_r)\)
  m a
prod kt x1 x2 y1 y2 = stToPrim $ prodST kt x1 x2 y1 y2

-- | \(O(1)\) Returns monoid product of all the points.
--
-- @since 1.2.2.0
{-# INLINE allProd #-}
allProd ::
  (PrimMonad m, Monoid a, VU.Unbox a) =>
  -- | `LazyKdTree`
  LazyKdTree (PrimState m) f a ->
  -- | Monoid product in the whole space.
  m a
allProd kt = do
  -- In case of zero vertices, use `Maybe`:
  fromMaybe mempty <$> VGM.readMaybe (dataLkt kt) 1

-- | \(O(\log n)\) Applies a monoid action to points in \([x_l, x_r) \times [y_l, y_r)\).
--
-- @since 1.2.2.0
{-# INLINE applyIn #-}
applyIn ::
  (HasCallStack, PrimMonad m, Eq f, SegAct f a, VU.Unbox f, Monoid a, VU.Unbox a) =>
  -- | `LazyKdTree`
  LazyKdTree (PrimState m) f a ->
  -- | \(x_l\)
  Int ->
  -- | \(x_r\)
  Int ->
  -- | \(y_l\)
  Int ->
  -- | \(y_r\)
  Int ->
  -- | \(f\)
  f ->
  -- | Monadic tuple
  m ()
applyIn kt x1 x2 y1 y2 f = stToPrim $ applyInST kt 1 x1 x2 y1 y2 f

-- -------------------------------------------------------------------------------------------------
-- Private
-- -------------------------------------------------------------------------------------------------

{-# INLINEABLE buildST #-}
buildST :: forall s f a. (HasCallStack, Monoid f, VU.Unbox f, Semigroup a, VU.Unbox a) => VU.Vector Int -> VU.Vector Int -> VU.Vector a -> ST s (LazyKdTree s f a)
buildST xs0 ys0 vs0 = do
  let nLkt = VU.length xs0
  let !_ = ACIA.runtimeAssert (nLkt == VU.length ys0 && nLkt == VU.length vs0) "AtCoder.Extra.LazyKdTree.buildST: the length of `xs`, `ys` and `vs` must be equal"
  if nLkt == 0
    then do
      let logLkt = 0
      dataLkt <- VUM.new 0
      lazyLkt <- VUM.new 0
      let incRectsLkt = VU.empty
      let sizeLkt = VU.empty
      let posLkt = VU.empty
      pure LazyKdTree {..}
    else do
      let logLkt = countTrailingZeros $ ACIB.bitCeil (nLkt + 1)
      dataLkt <- VUM.unsafeNew (bit (logLkt + 1))
      lazyLkt <- VUM.replicate (bit logLkt) mempty
      incRectsVec <- VUM.replicate (bit (logLkt + 1)) (maxBound, minBound, maxBound, minBound)
      size <- VUM.unsafeNew (bit (logLkt + 1))
      pos <- VUM.unsafeNew nLkt
      let VUM.MV_4 _ xMins xMaxes yMins yMaxes = incRectsVec

      -- - idx: rectangle index (one-based)
      -- - xs, ys, vs: point information (x, y and monoid value)
      -- - ids: maps sorted vertices to the original vertex indices
      -- - divX: represents hyperplane direction for point partition
      let buildSubtree :: Int -> VU.Vector Int -> VU.Vector Int -> VU.Vector a -> VU.Vector Int -> Bool -> ST s ()
          buildSubtree idx xs ys vs ids divX = do
            let n = VU.length xs
            VGM.write size idx n

            -- retrieve the bounds:
            let (!xMin, !xMax, !yMin, !yMax) =
                  VU.foldl'
                    (\(!a, !b, !c, !d) (!x, !y) -> (min a x, max b x, min c y, max d y))
                    (maxBound, minBound, maxBound, minBound)
                    $ VU.zip xs ys
            VGM.modify xMins (min xMin) idx
            VGM.modify xMaxes (max xMax) idx
            VGM.modify yMins (min yMin) idx
            VGM.modify yMaxes (max yMax) idx

            if n == 1
              then do
                -- it's a terminal. note that it's not always a leaf; the case is handled carefully in
                -- other methods
                VGM.write dataLkt idx $ vs VG.! 0
                -- record original vertex index -> rectangle index
                VGM.write pos (ids VG.! 0) idx
              else do
                -- partition the vertices into two:
                let m = n `div` 2
                let is = VU.create $ do
                      vec <- VUM.generate n id
                      if divX
                        then VAI.selectBy (comparing (xs VG.!)) vec m
                        else VAI.selectBy (comparing (ys VG.!)) vec m
                      pure vec

                -- TODO: permute in-place?
                let (!xsL, !xsR) = VG.splitAt m $ VG.backpermute xs is
                let (!ysL, !ysR) = VG.splitAt m $ VG.backpermute ys is
                let (!vsL, !vsR) = VG.splitAt m $ VG.backpermute vs is
                let (!idsL, !idsR) = VG.splitAt m $ VG.backpermute ids is

                -- build the subtree:
                buildSubtree (2 * idx + 0) xsL ysL vsL idsL (not divX)
                buildSubtree (2 * idx + 1) xsR ysR vsR idsR (not divX)
                xl <- VGM.read dataLkt (2 * idx + 0)
                xr <- VGM.read dataLkt (2 * idx + 1)
                VGM.write dataLkt idx $! xl <> xr

      buildSubtree 1 xs0 ys0 vs0 (VU.generate nLkt id) True
      sizeLkt <- VU.unsafeFreeze size
      posLkt <- VU.unsafeFreeze pos
      incRectsLkt <- VU.unsafeFreeze incRectsVec
      pure LazyKdTree {..}

{-# INLINE applyAtST #-}
applyAtST :: (SegAct f a, VU.Unbox f, VU.Unbox a) => LazyKdTree s f a -> Int -> f -> ST s ()
applyAtST LazyKdTree {..} i f = do
  -- NOTE: Here we're asssuming each monoid value has length one. If you need a monoid of length
  -- zero, e.g., if you're just reserving new point insertion, you must not rely on
  -- `segActWithLength`. You might want to use `V2` instead of `Sum`.
  let len = sizeLkt VG.! i
  VGM.modify dataLkt (segActWithLength len f) i
  when (i < bit logLkt) $ do
    VGM.modify lazyLkt (f <>) i

-- TODO: consider `INLINE`?
{-# INLINE pushST #-}
pushST :: (SegAct f a, Eq f, VU.Unbox f, VU.Unbox a) => LazyKdTree s f a -> Int -> ST s ()
pushST kt@LazyKdTree {..} i = do
  lazy <- VGM.read lazyLkt i
  unless (lazy == mempty) $ do
    applyAtST kt (2 * i + 0) lazy
    applyAtST kt (2 * i + 1) lazy
    VGM.write lazyLkt i mempty

{-# INLINEABLE prodST #-}
prodST :: (HasCallStack, SegAct f a, Eq f, VU.Unbox f, Monoid a, VU.Unbox a) => LazyKdTree s f a -> Int -> Int -> Int -> Int -> ST s a
prodST kt@LazyKdTree {..} x1 x2 y1 y2
  | x1 >= x2 || y1 >= y2 = pure mempty
  | otherwise = inner 1
  where
    inner i = case incRectsLkt VG.!? i of
      Nothing -> pure mempty
      Just (!xl, !xr, !yl, !yr)
        -- TODO: what is this?
        | xl > xr -> pure mempty
        -- not intersecting
        | x2 <= xl || x1 > xr || y2 <= yl || y1 > yr -> pure mempty
        -- the rectangle is fully contained by the query:
        | x1 <= xl && xr < x2 && y1 <= yl && yr < y2 -> do
            VGM.read dataLkt i
        | otherwise -> do
            pushST kt i
            l <- inner (2 * i + 0)
            r <- inner (2 * i + 1)
            pure $! l <> r

{-# INLINEABLE applyInST #-}
applyInST :: (HasCallStack, SegAct f a, Eq f, VU.Unbox f, Monoid a, VU.Unbox a) => LazyKdTree s f a -> Int -> Int -> Int -> Int -> Int -> f -> ST s ()
applyInST kt@LazyKdTree {..} i0 x1 x2 y1 y2 f
  | x1 >= x2 || y1 >= y2 = pure ()
  | otherwise = inner i0
  where
    inner i = case incRectsLkt VG.!? i of
      Nothing -> pure mempty
      Just (!xl, !xr, !yl, !yr)
        | xl > xr -> pure ()
        -- not intersecting
        | x2 <= xl || x1 > xr || y2 <= yl || y1 > yr -> pure ()
        -- the rectangle is fully contained by the query:
        | x1 <= xl && xr < x2 && y1 <= yl && yr < y2 -> do
            applyAtST kt i f
        | otherwise -> do
            pushST kt i
            inner (2 * i + 0)
            inner (2 * i + 1)
            l <- VGM.read dataLkt (2 * i + 0)
            r <- VGM.read dataLkt (2 * i + 1)
            VGM.write dataLkt i $! l <> r
