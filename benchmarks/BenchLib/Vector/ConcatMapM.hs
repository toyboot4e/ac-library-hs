{-# LANGUAGE RecordWildCards #-}

-- | Fast modular multiplication by barrett reduction.
module BenchLib.Vector.ConcatMapM
  ( primConcatMapM1,
    concatMapM1,
    concatMapM2,
    primIConcatMapM1,
    iconcatMapM1,
    iconcatMapM2,
  )
where

import Control.Monad.Primitive (PrimMonad)
import Data.Vector.Fusion.Bundle.Monadic qualified as BundleM
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM

-- I expected these functions do not perform well with State monad, but
-- they're working as fast as IO and ST?

-- | https://github.com/haskell/vector/issues/416
--
-- With this wrapper
unstreamPrimM :: (PrimMonad m, VG.Vector v a) => BundleM.Bundle m u a -> m (v a)
{-# INLINE [1] unstreamPrimM #-}
unstreamPrimM s = VGM.munstream s >>= VG.unsafeFreeze

-- | PrimMonad only. This addes more constraints, but performs well with complex monads.
{-# INLINE primConcatMapM1 #-}
primConcatMapM1 ::
  (PrimMonad m, VG.Vector v a, VG.Vector v b) =>
  (a -> m (v b)) ->
  v a ->
  m (v b)
primConcatMapM1 f =
  unstreamPrimM
    . BundleM.concatVectors
    . BundleM.mapM f
    . BundleM.fromVector

{-# INLINE concatMapM1 #-}
concatMapM1 ::
  (Monad m, VG.Vector v a, VG.Vector v b) =>
  (a -> m (v b)) ->
  v a ->
  m (v b)
concatMapM1 f =
  VG.unstreamM
    . BundleM.concatVectors
    . BundleM.mapM f
    . BundleM.fromVector

{-# INLINE concatMapM2 #-}
concatMapM2 ::
  (Monad m, VG.Vector v a, VG.Vector v b) =>
  (a -> m (v b)) ->
  v a ->
  m (v b)
concatMapM2 f xs =
  VG.unstreamM
    . BundleM.concatVectors
    $ BundleM.generateM (VG.length xs) (f . VG.unsafeIndex xs)

-- | PrimMonad only. This addes more constraints, but performs well with complex monads.
{-# INLINE primIConcatMapM1 #-}
primIConcatMapM1 ::
  (PrimMonad m, VG.Vector v a, VG.Vector v b) =>
  (Int -> a -> m (v b)) ->
  v a ->
  m (v b)
primIConcatMapM1 f =
  unstreamPrimM
    . BundleM.concatVectors
    . BundleM.mapM (uncurry f)
    . BundleM.indexed
    . BundleM.fromVector

{-# INLINE iconcatMapM1 #-}
iconcatMapM1 ::
  (Monad m, VG.Vector v a, VG.Vector v b) =>
  (Int -> a -> m (v b)) ->
  v a ->
  m (v b)
iconcatMapM1 f =
  VG.unstreamM
    . BundleM.concatVectors
    . BundleM.mapM (uncurry f)
    . BundleM.indexed
    . BundleM.fromVector

{-# INLINE iconcatMapM2 #-}
iconcatMapM2 ::
  (Monad m, VG.Vector v a, VG.Vector v b) =>
  (Int -> a -> m (v b)) ->
  v a ->
  m (v b)
iconcatMapM2 f xs =
  VG.unstreamM
    . BundleM.concatVectors
    $ BundleM.generateM (VG.length xs) (\i -> f i (VG.unsafeIndex xs i))
