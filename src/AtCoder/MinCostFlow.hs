{-# LANGUAGE RecordWildCards #-}

-- | Minimu cot flow.
module AtCoder.MinCostFlow (McfGraph, new, new', addEdge, addEdge_, getEdge, unsafeFreezeEdges, freezeEdges, flow, slope) where

import AtCoder.Internal.Assert (runtimeAssert)
import AtCoder.Internal.Buffer qualified as ACB
import AtCoder.Internal.GrowVec qualified as ACGV
import AtCoder.Internal.Heap qualified as ACH
import AtCoder.Internal.McfCSR qualified as McfCSR
import Control.Monad (unless, when)
import Control.Monad.Fix (fix)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Foldable (for_)
import Data.Maybe (fromJust)
import Data.Primitive.MutVar (readMutVar)
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Base qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Stack (HasCallStack)

-- | Max flow graph.
data McfGraph s cap cost = McfGraph
  { -- | The number of vertices.
    nG :: {-# UNPACK #-} !Int,
    -- | fromVertex -> vector of @(from, to, cap, flow, cost)@.
    edgesG :: !(ACGV.GrowVec s (Int, Int, cap, cap, cost))
  }

-- | \(O(n^2)\)
new :: (VU.Unbox cap, VU.Unbox cost, PrimMonad m) => Int -> m (McfGraph (PrimState m) cap cost)
new nG = do
  new' nG 0

-- | \(O(n + e)\)
new' :: (VU.Unbox cap, VU.Unbox cost, PrimMonad m) => Int -> Int -> m (McfGraph (PrimState m) cap cost)
new' nG nEdges = do
  edgesG <- ACGV.new nEdges
  return McfGraph {..}

-- | Amortized \(O(1)\)
addEdge ::
  (HasCallStack, PrimMonad m, Num cap, Ord cap, VU.Unbox cap, Num cost, Ord cost, VU.Unbox cost) =>
  McfGraph (PrimState m) cap cost ->
  Int ->
  Int ->
  cap ->
  cost ->
  m Int
addEdge McfGraph {..} from to cap cost = do
  let !_ = runtimeAssert (0 <= from && from < nG) "from vertex out of bounds"
  let !_ = runtimeAssert (0 <= to && to < nG) "to vertex out of bounds"
  let !_ = runtimeAssert (0 <= cap) "capacity has to bigger than or equal to 0"
  let !_ = runtimeAssert (0 <= cost) "cost has to bigger than or equal to 0"
  m <- ACGV.length edgesG
  ACGV.pushBack edgesG (from, to, cap, 0, cost)
  return m

-- | Amortized \(O(1)\)
addEdge_ ::
  (HasCallStack, PrimMonad m, Num cap, Ord cap, VU.Unbox cap, Num cost, Ord cost, VU.Unbox cost) =>
  McfGraph (PrimState m) cap cost ->
  Int ->
  Int ->
  cap ->
  cost ->
  m ()
addEdge_ graph from to cap cost = do
  _ <- addEdge graph from to cap cost
  return ()

-- | \(O(1)\)
getEdge ::
  (HasCallStack, PrimMonad m, Num cap, Ord cap, VU.Unbox cap, Num cost, Ord cost, VU.Unbox cost) =>
  McfGraph (PrimState m) cap cost ->
  Int ->
  m (Int, Int, cap, cap, cost)
getEdge McfGraph {..} i = do
  m <- ACGV.length edgesG
  let !_ = runtimeAssert (0 <= m && m < nG) "vertex out of bounds"
  ACGV.read edgesG i

-- | \(O(1)\) Returns a vector of @(from, to, cap, flow, cost)@.
unsafeFreezeEdges ::
  (HasCallStack, PrimMonad m, Num cap, Ord cap, VU.Unbox cap, Num cost, Ord cost, VU.Unbox cost) =>
  McfGraph (PrimState m) cap cost ->
  m (VU.Vector (Int, Int, cap, cap, cost))
unsafeFreezeEdges McfGraph {..} = do
  ACGV.unsafeFreeze edgesG

-- | \(O(m)\) Returns a vector of @(from, to, cap, flow, cost)@.
freezeEdges ::
  (HasCallStack, PrimMonad m, Num cap, Ord cap, VU.Unbox cap, Num cost, Ord cost, VU.Unbox cost) =>
  McfGraph (PrimState m) cap cost ->
  m (VU.Vector (Int, Int, cap, cap, cost))
freezeEdges McfGraph {..} = do
  ACGV.freeze edgesG

-- | FIXME: \(O(n^2 m)\)
flow ::
  (HasCallStack, PrimMonad m, Integral cap, Ord cap, VU.Unbox cap, Integral cost, Ord cost, Bounded cost, VU.Unbox cost) =>
  McfGraph (PrimState m) cap cost ->
  Int ->
  Int ->
  cap ->
  m (cap, cost)
flow graph s t flowLimit =
  VU.last <$> slope graph s t flowLimit

-- | FIXME: \(O(n^2 m)\)
slope ::
  (HasCallStack, PrimMonad m, Integral cap, Ord cap, VU.Unbox cap, Integral cost, Ord cost, Bounded cost, VU.Unbox cost) =>
  McfGraph (PrimState m) cap cost ->
  Int ->
  Int ->
  cap ->
  m (VU.Vector (cap, cost))
slope McfGraph {..} s t flowLimit = do
  let !_ = runtimeAssert (0 <= s && s < nG) "start vertex out of bounds"
  let !_ = runtimeAssert (0 <= t && t < nG) "end vertex out of bounds"
  let !_ = runtimeAssert (s /= t) "start and end vertex have to be distict"

  edges@(VU.V_5 _ _ _ caps _ _) <- ACGV.unsafeFreeze edgesG
  (!edgeIdx, !g) <- McfCSR.build nG edges
  result <- internalSlopeMCF g nG s t flowLimit

  (VUM.MV_5 _ _ _ _ flows _) <- readMutVar $ ACGV.vecGV edgesG
  VU.iforM_ (VU.zip caps edgeIdx) $ \i (!cap1, !iEdge) -> do
    cap2 <- VGM.read (McfCSR.capCSR g) iEdge
    VGM.write flows i $! cap2 - cap1

  return result

internalSlopeMCF ::
  forall cap cost m.
  (HasCallStack, PrimMonad m, Integral cap, Ord cap, VU.Unbox cap, Integral cost, Ord cost, Bounded cost, VU.Unbox cost) =>
  McfCSR.CSR (PrimState m) cap cost ->
  Int ->
  Int ->
  Int ->
  cap ->
  m (VU.Vector (cap, cost))
internalSlopeMCF csr@McfCSR.CSR {..} n s t flowLimit = do
  duals <- VUM.unsafeNew n :: m (VUM.MVector (PrimState m) cost)
  dists <- VUM.unsafeNew n :: m (VUM.MVector (PrimState m) cost)

  prevE <- VUM.unsafeNew n :: m (VUM.MVector (PrimState m) Int)
  vis <- VUM.unsafeNew n :: m (VUM.MVector (PrimState m) Bool)

  -- FIXME: initial capacity or make it growable
  queMin <- ACB.new n :: m (ACB.Buffer (PrimState m) Int)
  heap <- ACH.new n :: m (ACH.Heap (PrimState m) (cost, Int))

  let dualRef = do
        for_ [0 .. n - 1] $ \i -> do
          VGM.write dists i $ maxBound @cost
        VGM.set vis False
        ACB.clear queMin
        ACH.clear heap

        -- TODO: what are we doing here?
        VGM.write dists s 0
        ACB.pushBack queMin s
        fix $ \loop -> do
          b1 <- ACB.null queMin
          b2 <- ACH.null heap
          when (not b1 || not b2) $ do
            v <-
              if not b1
                then do
                  fromJust <$> ACB.popBack queMin
                else do
                  (!_, !to) <- fromJust <$> ACH.pop heap
                  return to

            visV <- VGM.exchange vis v True
            unless (v == t) $ do
              unless visV $ do
                -- dist[v] = shortest(s, v) + dual[s] - dual[v]
                -- dist[v] >= 0 (all reduced cost are positive)
                -- dist[v] <= (n-1)C
                dualV <- VGM.read duals v
                distV <- VGM.read dists v
                let start = startCSR VG.! v
                VU.iforM_ (McfCSR.adj csr v) $ \di (!to, !rev, !cost) -> do
                  cap <- VGM.read capCSR $ start + di

                  unless (cap == 0) $ do
                    -- \|-dual[e.to] + dual[v]| <= (n-1)C
                    -- cost <= C - -(n-1)C + 0 = nC
                    cost' <- do
                      dualTo <- VGM.read duals to
                      return $! cost - dualTo + dualV
                    distTo <- VGM.read dists to
                    when (distTo - distV > cost') $ do
                      let !distTo' = distV + cost'
                      VGM.write dists to distTo'
                      VGM.write prevE to rev
                      if distTo' == distV
                        then ACB.pushBack queMin to
                        else ACH.push heap (distTo', to)

              loop -- TODO: use updated value

        visT <- VGM.read vis t
        when visT $ do
          distT <- VGM.read dists t
          vis' <- VU.unsafeFreeze vis
          dists' <- VU.unsafeFreeze dists
          VU.iforM_ (VU.zip vis' dists') $ \v (!visV, !distV) -> do
            when visV $ do
              VGM.modify dists (subtract (distT - distV)) v
          return ()
        return visT

  result <- ACGV.new 16
  prevE' <- VU.unsafeFreeze prevE

  let inner :: cap -> cost -> cost -> m ()
      inner flow cost prevCostPerFlow
        | flow >= flowLimit = return ()
        | otherwise = do
            b <- dualRef
            if not b
              then return ()
              else do

                let minC :: cap -> Int -> m cap
                    minC !acc v
                      | v == s = return acc
                      | otherwise = do
                        let prev = prevE' VG.! v
                        let rev = revCSR VG.! prev
                        cap <- VGM.read capCSR rev
                        minC (min acc cap) $ toCSR VG.! prev
                c <- minC (flowLimit - flow) t

                let subC :: Int -> m ()
                    subC v
                      | v == s = return ()
                      | otherwise = do
                        let prev = prevE' VG.! v
                        VGM.modify capCSR (+ c) prev
                        VGM.modify capCSR (subtract c) $ revCSR VG.! v
                        subC $ toCSR VG.! prev
                subC t

                d <- negate <$> VGM.read duals s
                when (prevCostPerFlow == d) $ do
                  ACGV.deleteBack result
                ACGV.pushBack result (flow, cost)

                inner (flow + c) (cost + fromIntegral c * d) d

  inner 0 0 (-1)
  ACGV.unsafeFreeze result
