-- | Fast modular multiplication by barrett reduction.
module BenchLib.Vector.MapAccumL
  ( mapAccumL1,
    mapAccumL2,
    mapAccumL3,
    mapAccumL4,
  )
where

import Control.Monad.Primitive (PrimMonad)
import Control.Monad.ST (ST, runST)
import Control.Monad.Trans.State.Strict (StateT, runState, runStateT, state)
import Data.Vector.Fusion.Bundle qualified as Bundle
import Data.Vector.Fusion.Bundle.Monadic qualified as BundleM
import Data.Vector.Fusion.Bundle.Size qualified as Bundle
import Data.Vector.Fusion.Stream.Monadic qualified as MS
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM

{-# INLINE mapAccumL1 #-}
mapAccumL1 ::
  (VG.Vector v a, VG.Vector v b) =>
  (s -> a -> (s, b)) ->
  s ->
  v a ->
  (s, v b)
mapAccumL1 f s0 xs = (\(!x, !s) -> (s, x)) $ runState (VG.mapM g xs) s0
  where
    g a =
      state
        ( \s ->
            let (!s', !b) = f s a
             in (b, s')
        )

{-# INLINE mapAccumL2 #-}
mapAccumL2 ::
  (VG.Vector v a, VG.Vector v b) =>
  (s -> a -> (s, b)) ->
  s ->
  v a ->
  (s, v b)
mapAccumL2 f s0 xs = runST $ do
  vec <- VGM.unsafeNew (VG.length xs)
  !res <-
    VG.ifoldM'
      ( \ !s i x -> do
          let (!s', !x') = f s x
          VGM.write vec i x'
          pure s'
      )
      s0
      xs
  vec' <- VG.unsafeFreeze vec
  pure (res, vec')

{-# INLINE mapAccumL3 #-}
mapAccumL3 ::
  forall v s a b.
  (VG.Vector v a, VG.Vector v b) =>
  (s -> a -> (s, b)) ->
  s ->
  v a ->
  (s, v b)
mapAccumL3 f s0 xs = (\(!x, !s) -> (s, x)) $ runST $ (`runStateT` s0) $ do
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
unstreamPrimM :: (PrimMonad m, VG.Vector v a) => BundleM.Bundle m u a -> m (v a)
{-# INLINE [1] unstreamPrimM #-}
unstreamPrimM s = VGM.munstream s >>= VG.unsafeFreeze

-- @cojna/iota. Note that it does not return state value.
{-# INLINE mapAccumL4 #-}
mapAccumL4 ::
  (VG.Vector v a, VG.Vector v b) =>
  (s -> a -> (s, b)) ->
  s ->
  v a ->
  v b
mapAccumL4 f x =
  VG.unstream
    . Bundle.inplace
      (streamAccumM (\s a -> pure (f s a)) x)
      Bundle.toMax
    . VG.stream

-- @cojna/iota
streamAccumM :: (Monad m) => (s -> a -> m (s, b)) -> s -> MS.Stream m a -> MS.Stream m b
streamAccumM f s0 (MS.Stream step x0) = MS.Stream step' (s0, x0)
  where
    step' (!s, x) = do
      r <- step x
      case r of
        MS.Yield a x' -> do
          (s', b) <- f s a
          return $ MS.Yield b (s', x')
        MS.Skip x' -> return $ MS.Skip (s, x')
        MS.Done -> return MS.Done
    {-# INLINE [0] step' #-}
{-# INLINE [1] streamAccumM #-}
