-- | Miscellaneous vector functions. These functions are __not__ the fastest implementations, but
-- fills in some lacking features.
--
-- @since 1.2.2.0
module AtCoder.Extra.Vector
  ( -- * Sort functions
    argsort,

    -- * Vector Utilities
    iconcatMap,
    concatMapM,
    iconcatMapM,
    mapAccumL,
    chunks,

    -- * Queries
    maxRangeSum,
    minRangeSum,
  )
where

-- TODO: maybe add lexicographic permutations, combinations, and subsequences.

import Control.Monad.Primitive (PrimMonad)
import Control.Monad.ST (ST, runST)
import Control.Monad.Trans.State.Strict (StateT, runStateT, state)
import Data.Vector qualified as V
import Data.Vector.Algorithms.Intro qualified as VAI
import Data.Vector.Fusion.Bundle qualified as Bundle
import Data.Vector.Fusion.Bundle.Monadic qualified as BundleM
import Data.Vector.Fusion.Stream.Monadic qualified as S
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import GHC.Stack (HasCallStack)

-- | \(O(n \log n)\) Returns indices of the vector elements, stably sorted by their value.
--
-- ==== Example
-- >>> import AtCoder.Extra.Vector qualified as EV
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> EV.argsort $ VU.fromList @Int [0, 1, 0, 1, 0]
-- [0,2,4,1,3]
--
-- @since 1.2.3.0
{-# INLINEABLE argsort #-}
-- TODO: use generic vector
argsort :: (HasCallStack, Ord a, VU.Unbox a) => VU.Vector a -> VU.Vector Int
argsort xs =
  VU.modify
    (VAI.sortBy (\i j -> compare (xs VG.! i) (xs VG.! j) <> compare i j))
    $ VU.generate (VU.length xs) id

-- | Maps each element to a vector and concatenate the results.
--
-- ==== Example
-- >>> import AtCoder.Extra.Vector qualified as EV
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> EV.iconcatMap (\i x -> VU.fromList [i + x, i + x]) $ VU.replicate @Int 3 0
-- [0,0,1,1,2,2]
--
-- @since 1.5.1.0
{-# INLINE iconcatMap #-}
iconcatMap :: (HasCallStack, VG.Vector v a, VG.Vector v b) => (Int -> a -> v b) -> v a -> v b
iconcatMap f =
  VG.unstream
    . Bundle.concatVectors
    . Bundle.inplace (S.map (uncurry f) . S.indexed) id
    . VG.stream

-- | Maps each element to a vector and concatenate the results.
--
-- ==== Example
-- >>> import AtCoder.Extra.Vector qualified as EV
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> EV.iconcatMap (\x -> pure (VU.fromList [x, x])) $ VU.generate @Int 3 id
-- [0,0,1,1,2,2]
--
-- @since 1.5.1.0
{-# INLINE concatMapM #-}
concatMapM ::
  (HasCallStack, Monad m, VG.Vector v a, VG.Vector v b) =>
  (a -> m (v b)) ->
  v a ->
  m (v b)
concatMapM f =
  VG.unstreamM
    . BundleM.concatVectors
    . BundleM.mapM f
    . BundleM.fromVector

-- | Maps each element to a vector and concatenate the results.
--
-- ==== Example
-- >>> import AtCoder.Extra.Vector qualified as EV
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> EV.iconcatMapM (\i x -> pure (VU.fromList [i + x, i + x])) $ VU.replicate @Int 3 0
-- [0,0,1,1,2,2]
--
-- @since 1.5.1.0
{-# INLINE iconcatMapM #-}
iconcatMapM ::
  (HasCallStack, Monad m, VG.Vector v a, VG.Vector v b) =>
  (Int -> a -> m (v b)) ->
  v a ->
  m (v b)
iconcatMapM f =
  VG.unstreamM
    . BundleM.concatVectors
    . BundleM.mapM (uncurry f)
    . BundleM.indexed
    . BundleM.fromVector

-- | \(O(n)\) Maps a vector with an accumulator.
--
-- ==== Example
-- >>> import AtCoder.Extra.Vector qualified as EV
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> EV.mapAccumL (\s x -> (s + 1, s * x)) (0 :: Int) $ VU.generate @Int 4 id
-- (4,[0,1,4,9])
--
-- @since 1.5.1.0
{-# INLINE mapAccumL #-}
mapAccumL ::
  forall v s a b.
  (HasCallStack, VG.Vector v a, VG.Vector v b) =>
  (s -> a -> (s, b)) ->
  s ->
  v a ->
  (s, v b)
mapAccumL f s0 xs = (\(!x, !s) -> (s, x)) $ runST $ (`runStateT` s0) $ do
  unstreamPrimM
    . BundleM.mapM g
    $ BundleM.fromVector xs
  where
    g :: forall st. a -> StateT s (ST st) b
    g a =
      state
        ( \s ->
            let (!s', !b) = f s a
             in (b, s')
        )

-- | https://github.com/haskell/vector/issues/416
{-# INLINE [1] unstreamPrimM #-}
unstreamPrimM :: (PrimMonad m, VG.Vector v a) => BundleM.Bundle m u a -> m (v a)
unstreamPrimM s = VGM.munstream s >>= VG.unsafeFreeze

-- | \(O(n)\) Converts a vector into chunks of vectors with lenth \(k\). The last vector may have
-- smaller length than \(k\).
--
-- >>> import AtCoder.Extra.Vector qualified as EV
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> EV.chunks 3 $ VU.fromList ([1, 2, 3, 4, 5, 6, 7] :: [Int])
-- [[1,2,3],[4,5,6],[7]]
--
-- @since 1.5.1.0
{-# INLINE chunks #-}
chunks :: (VG.Vector v a) => Int -> v a -> V.Vector (v a)
chunks len xs0 = V.unfoldrExactN n step xs0
  where
    n = (VG.length xs0 + len - 1) `div` len
    step xs = (VG.take len xs, VG.drop len xs)

-- | \(O(n)\) Returns maximum range sum.
--
-- ==== Example
-- >>> import AtCoder.Extra.Vector qualified as EV
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> EV.maxRangeSum $ VU.fromList @Int [-3, 1, 6, -2, 7, -5]
-- 12
--
-- @since 1.5.1.0
{-# INLINEABLE maxRangeSum #-}
maxRangeSum :: forall v a. (VG.Vector v a, Ord a, Num a) => v a -> a
maxRangeSum xs = fst $ VG.foldl' f (0 :: a, 0 :: a) csum
  where
    csum = VG.postscanl' (+) (0 :: a) xs
    f (!acc, !minL) x = (max acc (x - minL), min minL x)

-- | \(O(n)\) Returns minimum range sum.
--
-- ==== Example
-- >>> import AtCoder.Extra.Vector qualified as EV
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> EV.minRangeSum $ VU.fromList @Int[-3, 1, 6, -20, 7, -9]
-- -22
--
-- @since 1.5.1.0
{-# INLINEABLE minRangeSum #-}
minRangeSum :: forall v a. (VG.Vector v a, Ord a, Num a) => v a -> a
minRangeSum xs = fst $ VG.foldl' f (0 :: a, 0 :: a) csum
  where
    csum = VG.postscanl' (+) (0 :: a) xs
    f (!acc, !maxL) x = (min acc (x - maxL), max maxL x)

-- TODO: add Swag etc?
