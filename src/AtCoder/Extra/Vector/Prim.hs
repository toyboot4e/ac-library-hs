-- | Monadic vector functions with @PrimMonad m@ constraints. They can fuse well.
--
-- Related issue: https://www.github.com/haskell/vector/issues/416.
--
-- Note that these functions are not 100% guaranteed to be sound.
--
-- @since 1.5.2.0
module AtCoder.Extra.Vector.Prim
  ( -- * Construction

    -- ** Monadic initialization
    replicateM,
    generateM,
    iterateNM,

    -- ** Unfolding

    -- TODO: implement unfoldrM
    constructNM,
    constructrNM,

    -- * Elementwise operations

    -- ** Monadic mapping
    mapM,
    mapM_,
    imapM,
    imapM_,
    forM,
    forM_,
    iforM,
    iforM_,

    -- ** Monadic zipping
    zipWithM,
    zipWithM_,
    izipWithM,
    izipWithM_,

    -- * Concat map
    concatMapM,
    iconcatMapM,

    -- * Working with predicates

    -- ** Filtering
    filterM,
    mapMaybeM,
    imapMaybeM,

    -- ** Monadic scanl
    prescanlM,
    prescanlM',
    postscanlM,
    postscanlM',
    scanlM,
    scanlM',
    scanl1M,
    scanl1M',
  )
where

import Control.Monad.Primitive (PrimMonad)
import Data.Vector.Fusion.Bundle qualified as Bundle
import Data.Vector.Fusion.Bundle.Monadic qualified as BundleM
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import GHC.Stack (HasCallStack)
import Prelude hiding (mapM, mapM_)

-- | ==== Constraints
-- - \(n \ge 0\)
--
-- @since 1.5.2.0
{-# INLINE replicateM #-}
replicateM :: (PrimMonad m, VG.Vector v a) => Int -> m a -> m (v a)
replicateM n f = unstreamPrimM (BundleM.replicateM n f)

-- | ==== Constraints
-- - \(n \ge 0\)
--
-- @since 1.5.2.0
{-# INLINE generateM #-}
generateM :: (PrimMonad m, VG.Vector v a) => Int -> (Int -> m a) -> m (v a)
generateM n f = unstreamPrimM (BundleM.generateM n f)

-- | ==== Constraints
-- - \(n \ge 0\)
--
-- @since 1.5.2.0
{-# INLINE iterateNM #-}
iterateNM :: (PrimMonad m, VG.Vector v a) => Int -> (a -> m a) -> a -> m (v a)
iterateNM n f x = unstreamPrimM (BundleM.iterateNM n f x)

-- | ==== Constraints
-- - \(n \ge 0\)
--
-- @since 1.5.2.0
{-# INLINE constructNM #-}
constructNM :: forall m v a. (PrimMonad m, VG.Vector v a) => Int -> (v a -> m a) -> m (v a)
constructNM n f = do
  v <- VGM.new n
  v' <- VG.unsafeFreeze v
  fill v' 0
  where
    fill :: v a -> Int -> m (v a)
    fill !v i
      | i < n = do
          x <- f (VG.unsafeTake i v)
          VG.elemseq v x $ do
            v' <- VG.unsafeThaw v
            VGM.unsafeWrite v' i x
            v'' <- VG.unsafeFreeze v'
            fill v'' (i + 1)
    fill v _ = pure v

-- | ==== Constraints
-- - \(n \ge 0\)
--
-- @since 1.5.2.0
{-# INLINE constructrNM #-}
constructrNM :: forall m v a. (PrimMonad m, VG.Vector v a) => Int -> (v a -> m a) -> m (v a)
constructrNM n f = do
  v <- n `seq` VGM.new n
  v' <- VG.unsafeFreeze v
  fill v' 0
  where
    fill :: v a -> Int -> m (v a)
    fill !v i
      | i < n = do
          x <- f (VG.unsafeSlice (n - i) i v)
          VG.elemseq v x $ do
            v' <- VG.unsafeThaw v
            VGM.unsafeWrite v' (n - i - 1) x
            v'' <- VG.unsafeFreeze v'
            fill v'' (i + 1)
    fill v _ = pure v

-- | @since 1.5.2.0
{-# INLINE mapM #-}
mapM :: (PrimMonad m, VG.Vector v a, VG.Vector v b) => (a -> m b) -> v a -> m (v b)
mapM f = unstreamPrimM . Bundle.mapM f . VG.stream

-- | @since 1.5.2.0
{-# INLINE imapM #-}
imapM :: (PrimMonad m, VG.Vector v a, VG.Vector v b) => (Int -> a -> m b) -> v a -> m (v b)
imapM f = unstreamPrimM . Bundle.mapM (uncurry f) . Bundle.indexed . VG.stream

-- | @since 1.5.2.0
{-# INLINE mapM_ #-}
mapM_ :: (PrimMonad m, VG.Vector v a) => (a -> m b) -> v a -> m ()
mapM_ f = Bundle.mapM_ f . VG.stream

-- | @since 1.5.2.0
{-# INLINE imapM_ #-}
imapM_ :: (PrimMonad m, VG.Vector v a) => (Int -> a -> m b) -> v a -> m ()
imapM_ f = Bundle.mapM_ (uncurry f) . Bundle.indexed . VG.stream

-- | @since 1.5.2.0
{-# INLINE forM #-}
forM :: (PrimMonad m, VG.Vector v a, VG.Vector v b) => v a -> (a -> m b) -> m (v b)
forM as f = mapM f as

-- | @since 1.5.2.0
{-# INLINE forM_ #-}
forM_ :: (PrimMonad m, VG.Vector v a) => v a -> (a -> m b) -> m ()
forM_ as f = mapM_ f as

-- | @since 1.5.2.0
iforM :: (PrimMonad m, VG.Vector v a, VG.Vector v b) => v a -> (Int -> a -> m b) -> m (v b)
{-# INLINE iforM #-}
iforM as f = imapM f as

-- | @since 1.5.2.0
{-# INLINE iforM_ #-}
iforM_ :: (PrimMonad m, VG.Vector v a) => v a -> (Int -> a -> m b) -> m ()
iforM_ as f = imapM_ f as

-- | @since 1.5.2.0
{-# INLINE zipWithM #-}
zipWithM :: (PrimMonad m, VG.Vector v a, VG.Vector v b, VG.Vector v c) => (a -> b -> m c) -> v a -> v b -> m (v c)
zipWithM f = \as bs -> unstreamPrimM $ Bundle.zipWithM f (VG.stream as) (VG.stream bs)

-- | @since 1.5.2.0
{-# INLINE izipWithM #-}
izipWithM :: (PrimMonad m, VG.Vector v a, VG.Vector v b, VG.Vector v c) => (Int -> a -> b -> m c) -> v a -> v b -> m (v c)
izipWithM m as bs = unstreamPrimM . Bundle.zipWithM (uncurry m) (Bundle.indexed (VG.stream as)) $ VG.stream bs

-- | @since 1.5.2.0
{-# INLINE zipWithM_ #-}
zipWithM_ :: (PrimMonad m, VG.Vector v a, VG.Vector v b) => (a -> b -> m c) -> v a -> v b -> m ()
zipWithM_ f = \as bs -> Bundle.zipWithM_ f (VG.stream as) (VG.stream bs)

-- | @since 1.5.2.0
{-# INLINE izipWithM_ #-}
izipWithM_ :: (PrimMonad m, VG.Vector v a, VG.Vector v b) => (Int -> a -> b -> m c) -> v a -> v b -> m ()
izipWithM_ m as bs = Bundle.zipWithM_ (uncurry m) (Bundle.indexed (VG.stream as)) $ VG.stream bs

-- | Maps each element to a vector and concatenate the results.
--
-- @since 1.5.2.0
{-# INLINE concatMapM #-}
concatMapM ::
  (HasCallStack, PrimMonad m, VG.Vector v a, VG.Vector v b) =>
  (a -> m (v b)) ->
  v a ->
  m (v b)
concatMapM f =
  unstreamPrimM
    . BundleM.concatVectors
    . BundleM.mapM f
    . BundleM.fromVector

-- | Maps each element to a vector and concatenate the results.
--
-- @since 1.5.2.0
{-# INLINE iconcatMapM #-}
iconcatMapM ::
  (HasCallStack, PrimMonad m, VG.Vector v a, VG.Vector v b) =>
  (Int -> a -> m (v b)) ->
  v a ->
  m (v b)
iconcatMapM f =
  unstreamPrimM
    . BundleM.concatVectors
    . BundleM.mapM (uncurry f)
    . BundleM.indexed
    . BundleM.fromVector

-- | @since 1.5.2.0
{-# INLINE filterM #-}
filterM :: (PrimMonad m, VG.Vector v a) => (a -> m Bool) -> v a -> m (v a)
filterM f = unstreamPrimM . Bundle.filterM f . VG.stream

-- | @since 1.5.2.0
mapMaybeM :: (PrimMonad m, VG.Vector v a, VG.Vector v b) => (a -> m (Maybe b)) -> v a -> m (v b)
{-# INLINE mapMaybeM #-}
mapMaybeM f = unstreamPrimM . Bundle.mapMaybeM f . VG.stream

-- | @since 0.12.2.0
{-# INLINE imapMaybeM #-}
imapMaybeM :: (PrimMonad m, VG.Vector v a, VG.Vector v b) => (Int -> a -> m (Maybe b)) -> v a -> m (v b)
imapMaybeM f = unstreamPrimM . Bundle.mapMaybeM (\(i, a) -> f i a) . Bundle.indexed . VG.stream

-- | https://github.com/haskell/vector/issues/416
{-# INLINE [1] unstreamPrimM #-}
unstreamPrimM :: (PrimMonad m, VG.Vector v a) => BundleM.Bundle m u a -> m (v a)
unstreamPrimM s = VGM.munstream s >>= VG.unsafeFreeze

-- | @since 1.5.2.0
{-# INLINE prescanlM #-}
prescanlM :: (HasCallStack, PrimMonad m, VG.Vector v a, VG.Vector v b) => (a -> b -> m a) -> a -> v b -> m (v a)
prescanlM f x0 =
  unstreamPrimM
    . prescanlMB f x0
    . VG.stream

-- | @since 1.5.2.0
{-# INLINE prescanlM' #-}
prescanlM' :: (HasCallStack, PrimMonad m, VG.Vector v a, VG.Vector v b) => (a -> b -> m a) -> a -> v b -> m (v a)
prescanlM' f x0 =
  unstreamPrimM
    . prescanlMB' f x0
    . VG.stream

-- | @since 1.5.2.0
{-# INLINE postscanlM #-}
postscanlM :: (HasCallStack, PrimMonad m, VG.Vector v a, VG.Vector v b) => (a -> b -> m a) -> a -> v b -> m (v a)
postscanlM f x0 =
  unstreamPrimM
    . postscanlMB f x0
    . VG.stream

-- | @since 1.5.2.0
{-# INLINE postscanlM' #-}
postscanlM' :: (HasCallStack, PrimMonad m, VG.Vector v a, VG.Vector v b) => (a -> b -> m a) -> a -> v b -> m (v a)
postscanlM' f x0 =
  unstreamPrimM
    . postscanlMB' f x0
    . VG.stream

-- | @since 1.5.2.0
{-# INLINE scanlM #-}
scanlM :: (HasCallStack, PrimMonad m, VG.Vector v a, VG.Vector v b) => (a -> b -> m a) -> a -> v b -> m (v a)
scanlM f x0 =
  unstreamPrimM
    . scanlMB f x0
    . VG.stream

-- | @since 1.5.2.0
{-# INLINE scanlM' #-}
scanlM' :: (HasCallStack, PrimMonad m, VG.Vector v a, VG.Vector v b) => (a -> b -> m a) -> a -> v b -> m (v a)
scanlM' f x0 =
  unstreamPrimM
    . scanlMB' f x0
    . VG.stream

-- | @since 1.5.2.0
{-# INLINE scanl1M #-}
scanl1M :: (HasCallStack, PrimMonad m, VG.Vector v a) => (a -> a -> m a) -> v a -> m (v a)
scanl1M f =
  unstreamPrimM
    . scanl1MB f
    . VG.stream

-- | @since 1.5.2.0
{-# INLINE scanl1M' #-}
scanl1M' :: (HasCallStack, PrimMonad m, VG.Vector v a) => (a -> a -> m a) -> v a -> m (v a)
scanl1M' f =
  unstreamPrimM
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
