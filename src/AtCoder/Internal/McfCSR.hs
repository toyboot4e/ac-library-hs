{-# LANGUAGE RecordWildCards #-}

-- | CSR for min cost flow.
module AtCoder.Internal.McfCSR (CSR (..), build, adj) where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Base qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Stack (HasCallStack)

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
build :: (HasCallStack, Num cap, VU.Unbox cap, VU.Unbox cost, Num cost, PrimMonad m) => Int -> VU.Vector (Int, Int, cap, cap, cost) -> m (VU.Vector Int, CSR (PrimState m) cap cost)
build n edges = do
  let m = VU.length edges
  -- craete the offsets first (this is a different step from ac-librar)
  let startCSR = VU.create $ do
        start <- VUM.replicate (n + 1) (0 :: Int)
        -- count degrees
        let (VU.V_5 _ froms tos _ _ _) = edges
        VU.forM_ (VU.zip froms tos) $ \(!from, !to) -> do
          VGM.modify start (+ 1) $ from + 1
          VGM.modify start (+ 1) $ to + 1
        -- sum up the degrees
        VUM.iforM_ (VUM.init start) $ \i dx -> do
          VGM.modify start (+ dx) (i + 1)
        return start

  toVec <- VUM.unsafeNew $ 2 * m
  revVec <- VUM.unsafeNew $ 2 * m
  capCSR <- VUM.unsafeNew $ 2 * m
  costVec <- VUM.unsafeNew $ 2 * m

  -- build CSR
  counter <- VU.thaw startCSR
  edgeIdx <- VU.forM edges $ \(!from, !to, !cap, !flow, !cost) -> do
    i1 <- VGM.read counter from
    VGM.modify counter (+ 1) from
    i2 <- VGM.read counter to
    VGM.modify counter (+ 1) to
    -- write forward edge
    VGM.write toVec i1 to
    VGM.write revVec i1 i2
    VGM.write capCSR i1 $! cap - flow
    VGM.write costVec i1 cost
    -- write backward edge
    VGM.write toVec i2 from
    VGM.write revVec i2 i1
    VGM.write capCSR i2 flow
    VGM.write costVec i2 (-cost)
    -- remember forward edge index
    return i1

  toCSR <- VU.unsafeFreeze toVec
  revCSR <- VU.unsafeFreeze revVec
  costCSR <- VU.unsafeFreeze costVec
  return (edgeIdx, CSR {..})

-- | \(O(1)\) Returns a vector of @(to, rev, cost)@.
adj :: (HasCallStack, Num cap, VU.Unbox cap, VU.Unbox cost) => CSR s cap cost -> Int -> VU.Vector (Int, Int, cost)
adj CSR {..} v = VU.slice offset len vec
  where
    offset = startCSR VG.! v
    len = startCSR VG.! (v + 1) - offset
    vec = VU.zip3 toCSR revCSR costCSR
