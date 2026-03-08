-- | Miscellaneous vector functions. These functions are __not__ the fastest implementations, but
-- fills in some lacking features.
--
-- @since 1.2.2.0
module AtCoder.Extra.Vector
  ( -- * Sort functions
    argsort,

    -- * Index compression
    compress,

    -- * Vector Utilities
    iconcatMap,
    concatMapM,
    iconcatMapM,
    mapAccumL,
    chunks,

    -- ** Monadic scanl
    prescanlM,
    prescanlM',
    postscanlM,
    postscanlM',
    scanlM,
    scanlM',
    scanl1M,
    scanl1M',

    -- ** Monadic folds and loops
    forRM_,
    iforRM_,
    foldMapM',
    foldrM',
    foldrM'_,
    ifoldrM',
    ifoldrM'_,

    -- * Queries
    maxRangeSum,
    minRangeSum,
    slideMinIndices,
    slideMaxIndices,
  )
where

-- TODO: maybe add lexicographic permutations, combinations, and subsequences.

import AtCoder.Extra.Bisect (lowerBound)
import AtCoder.Internal.Assert qualified as ACIA
import AtCoder.Internal.Queue qualified as Q
import Control.Monad (when)
import Control.Monad.Fix (fix)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.ST (ST, runST)
import Control.Monad.Trans.State.Strict (StateT, runStateT, state)
import Data.Ord (Down (..))
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

-- | \(O(n \log n)\) One dimensional index compression: xs -> (nubSortXs, xs')
--
-- ==== Example
-- >>> import AtCoder.Extra.Bisect (lowerBound)
-- >>> import AtCoder.Extra.Vector qualified as EV
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> let xs = VU.fromList [0, 20, 40, 10, 30]
-- >>> let (dict, xs') = EV.compress xs
-- >>> xs'
-- [0,2,4,1,3]
-- >>> lowerBound dict 10
-- 1
--
-- @since 1.5.1.0
{-# INLINE compress #-}
compress :: VU.Vector Int -> (VU.Vector Int, VU.Vector Int)
compress xs = (dict, VG.map (lowerBound dict) xs)
  where
    !dict = VG.uniq $ VG.modify VAI.sort xs

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

-- | https://github.com/haskell/vector/issues/416
{-# INLINE [1] unstreamPrimM #-}
unstreamPrimM :: (PrimMonad m, VG.Vector v a) => BundleM.Bundle m u a -> m (v a)
unstreamPrimM s = VGM.munstream s >>= VG.unsafeFreeze

-- | @since 1.5.1.0
{-# INLINE prescanlM #-}
prescanlM :: (HasCallStack, Monad m, VG.Vector v a, VG.Vector v b) => (a -> b -> m a) -> a -> v b -> m (v a)
prescanlM f x0 =
  VG.unstreamM
    . prescanlMB f x0
    . VG.stream

-- | @since 1.5.1.0
{-# INLINE prescanlM' #-}
prescanlM' :: (HasCallStack, Monad m, VG.Vector v a, VG.Vector v b) => (a -> b -> m a) -> a -> v b -> m (v a)
prescanlM' f x0 =
  VG.unstreamM
    . prescanlMB' f x0
    . VG.stream

-- | @since 1.5.1.0
{-# INLINE postscanlM #-}
postscanlM :: (HasCallStack, Monad m, VG.Vector v a, VG.Vector v b) => (a -> b -> m a) -> a -> v b -> m (v a)
postscanlM f x0 =
  VG.unstreamM
    . postscanlMB f x0
    . VG.stream

-- | @since 1.5.1.0
{-# INLINE postscanlM' #-}
postscanlM' :: (HasCallStack, Monad m, VG.Vector v a, VG.Vector v b) => (a -> b -> m a) -> a -> v b -> m (v a)
postscanlM' f x0 =
  VG.unstreamM
    . postscanlMB' f x0
    . VG.stream

-- | @since 1.5.1.0
{-# INLINE scanlM #-}
scanlM :: (HasCallStack, Monad m, VG.Vector v a, VG.Vector v b) => (a -> b -> m a) -> a -> v b -> m (v a)
scanlM f x0 =
  VG.unstreamM
    . scanlMB f x0
    . VG.stream

-- | @since 1.5.1.0
{-# INLINE scanlM' #-}
scanlM' :: (HasCallStack, Monad m, VG.Vector v a, VG.Vector v b) => (a -> b -> m a) -> a -> v b -> m (v a)
scanlM' f x0 =
  VG.unstreamM
    . scanlMB' f x0
    . VG.stream

-- | @since 1.5.1.0
{-# INLINE scanl1M #-}
scanl1M :: (HasCallStack, Monad m, VG.Vector v a) => (a -> a -> m a) -> v a -> m (v a)
scanl1M f =
  VG.unstreamM
    . scanl1MB f
    . VG.stream

-- | @since 1.5.1.0
{-# INLINE scanl1M' #-}
scanl1M' :: (HasCallStack, Monad m, VG.Vector v a) => (a -> a -> m a) -> v a -> m (v a)
scanl1M' f =
  VG.unstreamM
    . scanl1MB' f
    . VG.stream

{-# INLINE prescanlMB #-}
prescanlMB :: (Monad m) => (a -> b -> m a) -> a -> Bundle.Bundle v b -> BundleM.Bundle m v a
prescanlMB f x0 = BundleM.prescanlM f x0 . Bundle.lift

{-# INLINE prescanlMB' #-}
prescanlMB' :: (Monad m) => (a -> b -> m a) -> a -> Bundle.Bundle v b -> BundleM.Bundle m v a
prescanlMB' f x0 = BundleM.prescanlM' f x0 . Bundle.lift

{-# INLINE postscanlMB #-}
postscanlMB :: (Monad m) => (a -> b -> m a) -> a -> Bundle.Bundle v b -> BundleM.Bundle m v a
postscanlMB f x0 = BundleM.postscanlM f x0 . Bundle.lift

{-# INLINE postscanlMB' #-}
postscanlMB' :: (Monad m) => (a -> b -> m a) -> a -> Bundle.Bundle v b -> BundleM.Bundle m v a
postscanlMB' f x0 = BundleM.postscanlM' f x0 . Bundle.lift

{-# INLINE scanlMB #-}
scanlMB :: (Monad m) => (a -> b -> m a) -> a -> Bundle.Bundle v b -> BundleM.Bundle m v a
scanlMB f x0 = BundleM.scanlM f x0 . Bundle.lift

{-# INLINE scanlMB' #-}
scanlMB' :: (Monad m) => (a -> b -> m a) -> a -> Bundle.Bundle v b -> BundleM.Bundle m v a
scanlMB' f x0 = BundleM.scanlM' f x0 . Bundle.lift

{-# INLINE scanl1MB #-}
scanl1MB :: (Monad m) => (a -> a -> m a) -> Bundle.Bundle v a -> BundleM.Bundle m v a
scanl1MB f = BundleM.scanl1M f . Bundle.lift

{-# INLINE scanl1MB' #-}
scanl1MB' :: (Monad m) => (a -> a -> m a) -> Bundle.Bundle v a -> BundleM.Bundle m v a
scanl1MB' f = BundleM.scanl1M' f . Bundle.lift

-- | /O(n)/ Monadic right loop.
--
-- ==== Example
-- >>> import AtCoder.Extra.Vector qualified as EV
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> EV.forRM_ (VU.fromList @Int [-3, 1, 2]) print
-- 2
-- 1
-- -3
--
-- @since 1.6.0.0
{-# INLINE forRM_ #-}
forRM_ :: (VG.Vector v a, Monad m) =>  v a -> (a -> m ()) -> m ()
forRM_ v f = loop (n - 1)
  where
    n = VG.length v
    loop i
      | i < 0 = pure ()
      | otherwise = do
          let a = VG.unsafeIndex v i
          f a
          loop (i - 1)

-- Does not compile, due to monad type mismatch
-- {-# INLINE forRM_ #-}
-- forRM_ :: (VG.Vector v a, Monad m) => (a -> m ()) -> v a -> m ()
-- forRM_ f v = BundleM.foldlM' (\() a -> f a) () $ VG.streamR v

-- | /O(n)/ Monadic right loop.
--
-- ==== Example
-- >>> import AtCoder.Extra.Vector qualified as EV
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> EV.iforRM_ (VU.fromList @Int [-30, 10, 20]) (\i a -> putStrLn (show i ++ "," ++ show a))
-- 2,20
-- 1,10
-- 0,-30
--
-- @since 1.6.0.0
iforRM_ :: (VG.Vector v a, Monad m) => v a -> (Int -> a -> m ()) -> m ()
iforRM_ v f = loop (n - 1)
  where
    n = VG.length v
    loop i
      | i < 0 = pure ()
      | otherwise = do
          let a = VG.unsafeIndex v i
          f i a
          loop (i - 1)

-- It does not compile, due to monadic type mismatch in Bundle
-- {-# INLINE iforRM_ #-}
-- iforRM_ :: (VG.Vector v a, Monad m) => (Int -> a -> m ()) -> v a -> m ()
-- iforRM_ f v =
--   BundleM.foldlM' (\() (!i, !a) -> f i a) ()
--     . Bundle.indexedR (VG.length v)
--     $ VG.streamR v

-- | /O(n)/ Map each element of the structure to a monoid and combine the results.
--
-- ==== Example
-- >>> import AtCoder.Extra.Vector qualified as EV
-- >>> import Data.Semigroup (Sum (..))
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> EV.foldMapM' (pure . Sum) (VU.fromList @Int [30, 10, 20])
-- Sum {getSum = 60}
--
-- @since 1.6.0.0
{-# INLINE foldMapM' #-}
foldMapM' :: (Monoid m, Monad mo, VG.Vector v a) => (a -> mo m) -> v a -> mo m
foldMapM' f = VG.foldM' (\ !acc a -> (acc <>) <$> f a) mempty

-- | /O(n)/ Monadic right fold with strict accumulator.
--
-- ==== Example
-- >>> import AtCoder.Extra.Vector qualified as EV
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> EV.foldrM' (\a b -> pure (a + b)) (0 :: Int) (VU.fromList @Int [30, 10, 20])
-- 60
--
-- @since 1.6.0.0
{-# INLINE foldrM' #-}
foldrM' :: (VG.Vector v a, Monad m) => (a -> b -> m b) -> b -> v a -> m b
foldrM' f z xs = Bundle.foldM' (flip f) z $ VG.streamR xs

-- | /O(n)/ Monadic right fold with strict accumulator.
--
-- @since 1.6.0.0
{-# INLINE foldrM'_ #-}
foldrM'_ :: (VG.Vector v a, Monad m) => (a -> b -> m b) -> b -> v a -> m ()
foldrM'_ f b0 v = do
  _ <- foldrM' f b0 v
  pure ()

-- | /O(n)/ Monadic right fold with strict accumulator.
--
-- ==== Example
-- >>> import AtCoder.Extra.Vector qualified as EV
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> EV.ifoldrM' (\i a b -> putStrLn (show (i, a, b)) >> pure (a + b)) (0 :: Int) (VU.fromList @Int [30, 10, 20])
-- (2,20,0)
-- (1,10,20)
-- (0,30,30)
-- 60
--
-- @since 1.6.0.0
{-# INLINE ifoldrM' #-}
ifoldrM' :: (VG.Vector v a, Monad m) => (Int -> a -> b -> m b) -> b -> v a -> m b
ifoldrM' f z v = Bundle.foldM' (\b (!i, !a) -> f i a b) z
  . Bundle.indexedR (VG.length v)
  $ VG.streamR v

-- | /O(n)/ Monadic right fold with strict accumulator.
--
-- @since 1.6.0.0
{-# INLINE ifoldrM'_ #-}
ifoldrM'_ :: (VG.Vector v a, Monad m) => (Int -> a -> b -> m b) -> b -> v a -> m ()
ifoldrM'_ f b0 v = do
  _ <- ifoldrM' f b0 v
  pure ()

-- | \(O(n)\) Returns maximum range sum.
--
-- ==== Example
-- >>> import AtCoder.Extra.Vector qualified as EV
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> EV.maxRangeSum $ VU.fromList @Int [-3, 1, 6, -2, 7, -5]
-- 12
--
-- @since 1.5.1.0
{-# INLINE maxRangeSum #-}
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
{-# INLINE minRangeSum #-}
minRangeSum :: forall v a. (VG.Vector v a, Ord a, Num a) => v a -> a
minRangeSum xs = fst $ VG.foldl' f (0 :: a, 0 :: a) csum
  where
    csum = VG.postscanl' (+) (0 :: a) xs
    f (!acc, !maxL) x = (min acc (x - maxL), max maxL x)

-- | \(O(N)\) Returns indices of minimum values in the windows with the specified length.
--
-- ==== Constraints
-- - \(k \gt 0\)
--
-- ==== Example
--
-- >>> slideMinIndices 3 (VU.fromList [0 .. 5])
-- [0,1,2,3]
-- >>> slideMinIndices 3 (VU.fromList [5, 4 .. 0])
-- [2,3,4,5]
--
-- @since 1.5.1.0
{-# INLINE slideMinIndices #-}
slideMinIndices :: (HasCallStack) => Int -> VU.Vector Int -> VU.Vector Int
slideMinIndices k xs
  | VU.null xs = VU.empty
  | k >= VU.length xs = VU.singleton $ VU.minIndex xs
  | otherwise = slideCmpIndicesOn Down k xs
  where
    !_ = ACIA.runtimeAssert (k > 0) "AtCoder.Extra.Vector.slideMinIndices: given non-positive k"

-- | \(O(N)\) Returns indices of maximum values in the windows with the specified length.
--
-- ==== Constraints
-- - \(k \gt 0\)
--
-- ==== Example
--
-- @
-- indices: 0 1 2 3 4 5
-- values:  0 1 2 3 4 5   max value indices:
--          [---]         2
--            [---]       3
--              [---]     4
--                [---]   5
-- @
--
-- >>> slideMaxIndices 3 (VU.fromList [0 .. 5])
-- [2,3,4,5]
-- >>> slideMaxIndices 3 (VU.fromList [5, 4 .. 0])
-- [0,1,2,3]
--
-- @since 1.5.1.0
{-# INLINE slideMaxIndices #-}
slideMaxIndices :: (HasCallStack) => Int -> VU.Vector Int -> VU.Vector Int
slideMaxIndices k xs
  | VU.null xs = VU.empty
  | k >= VU.length xs = VU.singleton $ VU.maxIndex xs
  | otherwise = slideCmpIndicesOn id k xs
  where
    !_ = ACIA.runtimeAssert (k > 0) "AtCoder.Extra.Vector.slideMaxIndices: given non-positive k"

-- | \(O(N)\) (1) in <https://qiita.com/kuuso1/items/318d42cd089a49eeb332>
{-# INLINE slideCmpIndicesOn #-}
slideCmpIndicesOn :: (VG.Vector v a, Ord b) => (a -> b) -> Int -> v a -> VU.Vector Int
slideCmpIndicesOn wrap len xs = runST $ do
  -- dequeue of maximum number indices.
  !buf <- Q.new (VG.length xs)

  fmap (VU.drop (len - 1)) $ VG.generateM (VG.length xs) $ \i -> do
    -- remove the front indices that are no longer in the span
    fix $ \loop -> do
      b <- maybe False (<= i - len) <$> Q.readMaybeFront buf 0
      when b $ do
        Q.popFront_ buf
        loop

    -- remove the last indices that are less attractive to the new coming value
    fix $ \loop -> do
      b <- maybe False ((< wrap (xs VG.! i)) . wrap . (xs VG.!)) <$> Q.readMaybeBack buf 0
      when b $ do
        Q.popBack_ buf
        loop

    Q.pushBack buf i
    Q.readFront buf 0
