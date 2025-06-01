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

import Control.Monad (guard)
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

-- | \(O(n \log n)\) Returns indices of the vector, stably sorted by their value.
--
-- ==== Example
-- >>> import Data.Vector.Algorithms.Intro qualified as VAI
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> argsort $ VU.fromList [0, 1, 0, 1, 0]
-- [0,2,4,1,3]
--
-- @since 1.2.3.0
{-# INLINEABLE argsort #-}
-- TODO: use generic vector
argsort :: (Ord a, VU.Unbox a) => VU.Vector a -> VU.Vector Int
argsort xs =
  VU.modify
    (VAI.sortBy (\i j -> compare (xs VG.! i) (xs VG.! j) <> compare i j))
    $ VU.generate (VU.length xs) id

-- | Map a function over a vector and concatenate the results.
--
-- @since 1.5.1.0
{-# INLINE iconcatMap #-}
iconcatMap :: (VG.Vector v a, VG.Vector v b) => (Int -> a -> v b) -> v a -> v b
iconcatMap f =
  VG.unstream
    . Bundle.concatVectors
    . Bundle.inplace (S.map (uncurry f) . S.indexed) id
    . VG.stream

-- | Map a function over a vector and concatenate the results.
--
-- @since 1.5.1.0
{-# INLINE concatMapM #-}
concatMapM ::
  (Monad m, VG.Vector v a, VG.Vector v b) =>
  (a -> m (v b)) ->
  v a ->
  m (v b)
concatMapM f =
  VG.unstreamM
    . BundleM.concatVectors
    . BundleM.mapM f
    . BundleM.fromVector

-- | Map a function over a vector and concatenate the results.
--
-- @since 1.5.1.0
{-# INLINE iconcatMapM #-}
iconcatMapM ::
  (Monad m, VG.Vector v a, VG.Vector v b) =>
  (Int -> a -> m (v b)) ->
  v a ->
  m (v b)
iconcatMapM f =
  VG.unstreamM
    . BundleM.concatVectors
    . BundleM.mapM (uncurry f)
    . BundleM.indexed
    . BundleM.fromVector

-- | Maps a vector with accumulator.
--
-- This function is ported from [cojna/iota](https://github.com/cojna/iota) (CC0 1.0 license).
--
-- @since 1.5.1.0
{-# INLINE mapAccumL #-}
mapAccumL ::
  forall v s a b.
  (VG.Vector v a, VG.Vector v b) =>
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
-- >>> chunks 3 $ U.fromList ([1, 2, 3, 4, 5, 6, 7] :: [Int])
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
-- @since 1.5.1.0
{-# INLINEABLE maxRangeSum #-}
maxRangeSum :: forall v a. (VG.Vector v a, Ord a, Num a) => v a -> a
maxRangeSum xs = fst $ VG.foldl' f (0 :: a, 0 :: a) csum
  where
    csum = VG.postscanl' (+) (0 :: a) xs
    f (!acc, !minL) x = (max acc (x - minL), min minL x)

-- | \(O(n)\) Returns maximum range sum.
--
-- @since 1.5.1.0
{-# INLINEABLE minRangeSum #-}
minRangeSum :: forall v a. (VG.Vector v a, Ord a, Num a) => v a -> a
minRangeSum xs = fst $ VG.foldl' f (0 :: a, 0 :: a) csum
  where
    csum = VG.postscanl' (+) (0 :: a) xs
    f (!acc, !maxL) x = (min acc (x - maxL), max maxL x)

-- TODO: add Swag etc?
