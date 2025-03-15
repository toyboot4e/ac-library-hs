{-# LANGUAGE RecordWildCards #-}

-- | Static, \(k\)-dimensional tree \((k = 2)\).
--
-- - Points are fixed on `build`.
-- - Multiple points can exist at the same coordinate.
--
-- ==== __Examples__
-- >>> import AtCoder.Extra.KdTree qualified as KT
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> let xys = VU.fromList [(0, 0), (1, 1), (4, 2)]
-- >>> let kt = KT.build2 xys
-- >>> -- Find point indices in [0, 2) x [0, 2) with maximum capacity 3
-- >>> KT.findPointsIn kt 0 2 0 2 3
-- [0,1]
--
-- >>> KT.findNearestPoint kt 3 3
-- Just 2
--
-- @since 1.2.2.0
module AtCoder.Extra.KdTree
  ( -- * K-dimensional tree
    KdTree (..),

    -- * Constructors
    build,
    build2,
    findPointsIn,
    findNearestPoint,
  )
where

import AtCoder.Internal.Assert qualified as ACIA
import AtCoder.Internal.Bit qualified as ACIB
import Control.Monad.ST (runST)
import Data.Bits
import Data.Ord (comparing)
import Data.Vector.Algorithms.Intro qualified as VAI
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Stack (HasCallStack)

-- | Static, \(k\)-dimensional tree \((k = 2)\).
--
-- @since 1.2.2.0
data KdTree = KdTree
  { -- | The number of points in the \(k\)-d tree.
    --
    -- @since 1.2.2.0
    nKt :: {-# UNPACK #-} !Int,
    -- | Rectangle information: inclusive (closed) ranges \([x_1, x_2) \times [y_1, y_2)\).
    --
    -- @since 1.2.2.0
    incRectsKt :: !(VU.Vector (Int, Int, Int, Int)),
    -- | Maps rectangle index to original point index.
    --
    -- @since 1.2.2.0
    dataKt :: !(VU.Vector Int)
  }

-- | \(O(n \log n)\) Creates a `KdTree` from \(x\) and \(y\) vectors.
--
-- ==== Constraints
-- - \(|\mathrm{xs}| = |\mathrm{ys}|\).
--
-- @since 1.2.2.0
{-# INLINEABLE build #-}
build ::
  (HasCallStack) =>
  -- | \(x\) coordnates
  VU.Vector Int ->
  -- | \(y\) coordnates
  VU.Vector Int ->
  -- | `KdTree`
  KdTree
build xs0 ys0 =
  let nKt = VU.length xs0
      !_ = ACIA.runtimeAssert (nKt == VU.length ys0) "AtCoder.Extra.KdTree.buildST: the length of `xs`, `ys` and `vs` must be equal"
   in if nKt == 0
        then KdTree 0 VU.empty VU.empty
        else runST $ do
          let vs0 = VU.generate nKt id
          let logKt = countTrailingZeros $ ACIB.bitCeil (nKt + 1)
          dat <- VUM.replicate (bit (logKt + 1)) (-1 :: Int)
          incRectsVec <- VUM.replicate (bit (logKt + 1)) (maxBound, minBound, maxBound, minBound)
          let VUM.MV_4 _ xMins xMaxes yMins yMaxes = incRectsVec

          -- - idx: rectangle index (one-based)
          -- - xs, ys, vs: point information (x, y and monoid value)
          -- - ids: maps sorted vertices to the original vertex indices
          -- - divX: represents hyperplane direction for point partition
          let -- buildSubtree :: Int -> VU.Vector Int -> VU.Vector Int -> VU.Vector Int -> VU.Vector Int -> Bool -> ST s ()
              buildSubtree idx xs ys vs ids divX = do
                let n = VU.length xs

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
                    -- it's a terminal
                    VGM.write dat idx $ vs VG.! 0
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

          buildSubtree 1 xs0 ys0 vs0 (VU.generate nKt id) True
          dataKt <- VU.unsafeFreeze dat
          incRectsKt <- VU.unsafeFreeze incRectsVec
          pure KdTree {..}

-- | \(O(n \log n)\) Creates `KdTree` from a \((x, y)\) vector.
--
-- ==== Constraints
-- - \(|\mathrm{xs}| = |\mathrm{ys}|\).
--
-- @since 1.2.2.0
{-# INLINE build2 #-}
build2 ::
  (HasCallStack) =>
  -- | \(x, y\) coordnates
  VU.Vector (Int, Int) ->
  -- | `KdTree`
  KdTree
build2 xys = build xs ys
  where
    (!xs, !ys) = VU.unzip xys

-- | \(O(n \log n)\) Collects points in \([x_l, x_r) \times [y_l, y_r)\).
--
-- @since 1.2.2.0
{-# INLINE findPointsIn #-}
findPointsIn ::
  (HasCallStack) =>
  -- | `KdTree`
  KdTree ->
  -- | \(x_l\)
  Int ->
  -- | \(x_r\)
  Int ->
  -- | \(y_l\)
  Int ->
  -- | \(y_r\)
  Int ->
  -- | Maximum number of points in \([x_l, x_r) \times [y_l, y_r)\).
  Int ->
  -- | Point indices in \([x_l, x_r) \times [y_l, y_r)\).
  VU.Vector Int
findPointsIn KdTree {..} x1 x2 y1 y2 capacity
  | nKt == 0 = VU.empty
  | otherwise = runST $ do
      res <- VUM.unsafeNew $ min nKt capacity
      let inner i iPush
            -- not intersected
            | x2 <= xMin || xMax < x1 = pure iPush
            | y2 <= yMin || yMax < y1 = pure iPush
            -- a leaf
            | vi /= -1 = do
                VGM.write res iPush vi
                pure $ iPush + 1
            -- a parental rectangle area
            | otherwise = do
                iPush' <- inner (2 * i + 0) iPush
                inner (2 * i + 1) iPush'
            where
              (!xMin, !xMax, !yMin, !yMax) = incRectsKt VG.! i
              vi = dataKt VG.! i
      n <- inner 1 0
      VU.take n <$> VU.unsafeFreeze res
  where
    !_ = ACIA.runtimeAssert (x1 <= x2 && y1 <= y2) "AtCoder.Extra.KdTree.findPointsIn: given invalid interval"

-- | \(O(\log n)\), only if the points are randomly distributed. Returns the index of the nearest
-- point, or `Nothing` if the `KdTree` has no point.
--
-- @since 1.2.2.0
{-# INLINE findNearestPoint #-}
findNearestPoint ::
  (HasCallStack) =>
  -- | `KdTree`
  KdTree ->
  -- | \(x\)
  Int ->
  -- | \(y\)
  Int ->
  -- | The nearest point index
  Maybe Int
findNearestPoint KdTree {..} x y
  | nKt == 0 = Nothing
  | otherwise = Just . fst $! inner 1 {- FIXME: -} (-1, -1)
  where
    clamp a aMin aMax = min aMax $ max a aMin
    -- Used for pruning. It's |(x, y)|^2 if the (x, y) is within the rectangle.
    bestDistSquared i =
      let (!xMin, !xMax, !yMin, !yMax) = incRectsKt VG.! i
          dx = x - clamp x xMin xMax
          dy = y - clamp y yMin yMax
       in dx * dx + dy * dy
    -- returns (index, bestDist)
    inner i res@(!resV, !resD)
      -- pruning (we have a better point than any point in this rectangle)
      | resV /= -1 && resD <= d = res
      -- it's a leaf
      | dataI /= -1 = (dataI, d)
      -- look into the children
      | d0 < d1 = inner (2 * i + 0) $ inner (2 * i + 1) res
      | otherwise = inner (2 * i + 1) $ inner (2 * i + 0) res
      where
        d = bestDistSquared i
        dataI = dataKt VG.! i
        d0 = bestDistSquared (2 * i + 0)
        d1 = bestDistSquared (2 * i + 0)
