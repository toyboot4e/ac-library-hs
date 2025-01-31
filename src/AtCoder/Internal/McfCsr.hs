{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_HADDOCK hide #-}

-- | Internal CSR for `AtCoder.MinCostFlow`.
--
-- @since 1.0.0.0
module AtCoder.Internal.McfCsr
  ( -- * Compressed sparse row
    Csr (..),

    -- * Constructor
    build,

    -- * Accessor
    adj,
  )
where

import Control.Monad.Primitive (PrimMonad, PrimState, stToPrim)
import Control.Monad.ST (ST)
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Base qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Stack (HasCallStack)

-- | CSR for min cost flow.
--
-- @since 1.0.0.0
data Csr s cap cost = Csr
  { -- | @since 1.0.0.0
    startCsr :: !(VU.Vector Int),
    -- | @since 1.0.0.0
    toCsr :: !(VU.Vector Int),
    -- | @since 1.0.0.0
    revCsr :: !(VU.Vector Int),
    -- | Mutable.
    --
    -- @since 1.0.0.0
    capCsr :: !(VUM.MVector s cap),
    -- | @since 1.0.0.0
    costCsr :: !(VU.Vector cost)
  }

{-# INLINEABLE buildST #-}
buildST :: (HasCallStack, Num cap, VU.Unbox cap, VU.Unbox cost, Num cost) => Int -> VU.Vector (Int, Int, cap, cap, cost) -> ST s (VU.Vector Int, Csr s cap cost)
buildST n edges = do
  let m = VU.length edges
  -- craete the offsets first (this is a different step from ac-librar)
  let startCsr = VU.create $ do
        start <- VUM.replicate (n + 1) (0 :: Int)
        -- count degrees
        let (VU.V_5 _ froms tos _ _ _) = edges
        VU.forM_ (VU.zip froms tos) $ \(!from, !to) -> do
          VGM.modify start (+ 1) $ from + 1
          VGM.modify start (+ 1) $ to + 1
        -- sum up the degrees
        VUM.iforM_ (VUM.init start) $ \i dx -> do
          VGM.modify start (+ dx) (i + 1)
        pure start

  toVec <- VUM.unsafeNew $ 2 * m
  revVec <- VUM.unsafeNew $ 2 * m
  capCsr <- VUM.unsafeNew $ 2 * m
  costVec <- VUM.unsafeNew $ 2 * m

  -- build CSR
  counter <- VU.thaw startCsr
  edgeIdx <- VU.forM edges $ \(!from, !to, !cap, !flow, !cost) -> do
    i1 <- VGM.read counter from
    VGM.modify counter (+ 1) from
    i2 <- VGM.read counter to
    VGM.modify counter (+ 1) to
    -- write forward edge
    VGM.write toVec i1 to
    VGM.write revVec i1 i2
    VGM.write capCsr i1 $! cap - flow
    VGM.write costVec i1 cost
    -- write backward edge
    VGM.write toVec i2 from
    VGM.write revVec i2 i1
    VGM.write capCsr i2 flow
    VGM.write costVec i2 (-cost)
    -- remember forward edge index
    pure i1

  toCsr <- VU.unsafeFreeze toVec
  revCsr <- VU.unsafeFreeze revVec
  costCsr <- VU.unsafeFreeze costVec
  pure (edgeIdx, Csr {..})

-- | \(O(n + m)\) Creates `Csr`.
--
-- @since 1.0.0.0
{-# INLINE build #-}
build :: (HasCallStack, PrimMonad m, Num cap, VU.Unbox cap, VU.Unbox cost, Num cost) => Int -> VU.Vector (Int, Int, cap, cap, cost) -> m (VU.Vector Int, Csr (PrimState m) cap cost)
build n edges = stToPrim $ buildST n edges

-- | \(O(1)\) Returns a vector of @(to, rev, cost)@.
--
-- @since 1.0.0.0
{-# INLINE adj #-}
adj :: (HasCallStack, Num cap, VU.Unbox cap, VU.Unbox cost) => Csr s cap cost -> Int -> VU.Vector (Int, Int, cost)
adj Csr {..} v = VU.slice offset len vec
  where
    offset = startCsr VG.! v
    len = startCsr VG.! (v + 1) - offset
    vec = VU.zip3 toCsr revCsr costCsr
