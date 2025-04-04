module BenchLib.SwapDupe
  ( swapDupeConcatMap,
    swapDupePP,
    swapDupeST,
  )
where

import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM

{-# INLINE swapDupeConcatMap #-}
swapDupeConcatMap :: (VU.Unbox w) => VU.Vector (Int, Int, w) -> VU.Vector (Int, Int, w)
swapDupeConcatMap = VU.concatMap (\(!u, !v, !w) -> VU.fromListN 2 [(u, v, w), (v, u, w)])

{-# INLINE swapDupePP #-}
swapDupePP :: (VU.Unbox w) => VU.Vector (Int, Int, w) -> VU.Vector (Int, Int, w)
swapDupePP uvws = uvws VU.++ VU.map (\(!u, !v, !w) -> (v, u, w)) uvws

{-# INLINEABLE swapDupeST #-}
swapDupeST :: (VU.Unbox w) => VU.Vector (Int, Int, w) -> VU.Vector (Int, Int, w)
swapDupeST uvws = VU.create $ do
  vec <- VUM.unsafeNew (2 * VU.length uvws)
  VU.iforM_ uvws $ \i (!u, !v, !w) -> do
    VGM.unsafeWrite vec (2 * i + 0) (u, v, w)
    VGM.unsafeWrite vec (2 * i + 1) (v, u, w)
  pure vec
