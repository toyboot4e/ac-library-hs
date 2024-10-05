{-# LANGUAGE RecordWildCards #-}

-- | CSR for min cost flow.
module AtCoder.Internal.McfCSR (CSR (..), build, adj) where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Foldable (for_)
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Base qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM

-- | CSR for min cost flow.
data CSR s cap cost = CSR
  { startCSR :: !(VU.Vector Int),
    toCSR :: !(VU.Vector Int),
    revCSR :: !(VU.Vector Int),
    -- | Mutable
    capCSR :: !(VUM.MVector s cap),
    costCSR :: !(VU.Vector cost)
  }

-- | \(O(n + m)\)
build :: (Num cap, VU.Unbox cap, VU.Unbox cost, PrimMonad m) => Int -> VU.Vector (Int, Int, cap, cap, cost) -> m (VU.Vector Int, CSR (PrimState m) cap cost)
build n edges = do
  let m = VU.length edges
  -- craete the offsets first (this is a different step from ac-librar)
  let startCSR = VU.create $ do
        start <- VUM.replicate (n + 1) (0 :: Int)
        -- count degrees
        let (VU.V_5 _ froms tos _ _ _) = edges
        VU.forM_ (VU.zip froms tos) $ \(!from, !to) -> do
          VUM.modify start (+ 1) $ from + 1
          VUM.modify start (+ 1) $ to + 1
        -- sum up
        for_ [1 .. n] $ \i -> do
          prev <- VGM.read start (i - 1)
          VGM.modify start (+ prev) i
        return start

  toVec <- VUM.unsafeNew m
  revVec <- VUM.unsafeNew (VU.length edges)
  capCSR <- VUM.unsafeNew m
  costVec <- VUM.unsafeNew m

  -- build CSR
  counter <- VU.thaw startCSR
  edgeIdx <- VUM.unsafeNew m
  VU.iforM_ edges $ \i (!from, !to, !cap, !flow, !cost) -> do
    i1 <- VGM.read counter from
    VGM.write edgeIdx i i1
    VGM.modify counter (+ 1) from
    i2 <- VGM.read counter to
    VGM.modify counter (+ 1) to
    -- forward edge
    VGM.write toVec from to
    VGM.write revVec from i2
    VGM.write capCSR from $! cap - flow
    VGM.write costVec from cost
    -- backward edge
    VGM.write toVec to from
    VGM.write revVec to i1
    VGM.write capCSR to flow
    VGM.write costVec to cost

  edgeIdx' <- VU.unsafeFreeze edgeIdx
  toCSR <- VU.unsafeFreeze toVec
  revCSR <- VU.unsafeFreeze toVec
  costCSR <- VU.unsafeFreeze costVec
  return (edgeIdx', CSR {..})

-- | \(O(1)\)
adj :: (Num cap, VU.Unbox cap, VU.Unbox cost) => CSR s cap cost -> Int -> VU.Vector (Int, Int, cost)
adj CSR {..} v = VU.unsafeSlice offset len vec
  where
    offset = startCSR VG.! v
    len = startCSR VG.! (v + 1) - offset
    vec = VU.zip3 toCSR revCSR costCSR
