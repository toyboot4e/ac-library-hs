{-# LANGUAGE RecordWildCards #-}

-- | It solves [Minimum-cost flow problem](https://en.wikipedia.org/wiki/Minimum-cost_flow_problem).
--
-- ==== __Example__
-- Create min cost flow graph (`McfGraph`):
--
-- >>> import AtCoder.MinCostFlow qualified as MCF
-- >>> g <- MCF.new @_ {- capacity -} @Int {- cost -} @Int 4
--
-- Build a simple graph with @'addEdge' g from to cap cost@ or `addEdge_`:
--
-- >>> MCF.addEdge g 0 1 2 3    --  0 --> 1     2
-- 0
-- >>> MCF.addEdge_ g 1 2 2 5   --  0 --> 1 --> 2
--
-- Augument flow with `flow`, `maxFlow` or `slope`:
--
-- >>> MCF.slope g 0 2 maxBound -- slope g from to flowLimit
-- [(0,0),(2,16)]
--
-- Note that you can't call `flow`, `maxFlow` or `slope` multiple times, or else you'll get wrong
-- return value.
module AtCoder.MinCostFlow
  ( -- * Minimum cost flow
    McfGraph (nG),

    -- * Constructor
    new,

    -- * Graph building
    addEdge,
    addEdge_,

    -- * Flow and slope
    flow,
    maxFlow,
    slope,

    -- * Edge information
    getEdge,
    edges,
    unsafeEdges,
  )
where

-- TODO: add `maxCostFlow`.
-- TODO: add `build`.

import AtCoder.Internal.Assert qualified as ACIA
import AtCoder.Internal.Buffer qualified as ACIB
import AtCoder.Internal.GrowVec qualified as ACIGV
import AtCoder.Internal.McfCsr qualified as ACIMCSR
import AtCoder.Internal.MinHeap qualified as ACIMH
import Control.Monad (unless, when)
import Control.Monad.Fix (fix)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Bit (Bit (..))
import Data.Maybe (fromJust)
import Data.Primitive.MutVar (readMutVar)
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Base qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Stack (HasCallStack)

-- | Min cost flow graph.
data McfGraph s cap cost = McfGraph
  { -- | The number of vertices.
    nG :: {-# UNPACK #-} !Int,
    -- | fromVertex -> vector of @(from, to, cap, flow, cost)@.
    edgesG :: !(ACIGV.GrowVec s (Int, Int, cap, cap, cost))
  }

-- | Creates a directed graph with \(n\) vertices and \(0\) edges. @cap@ and @cost@ are the type of
-- the capacity and the cost, respectively.
--
-- ==== Constraints
-- - \(0 \leq n\)
--
-- ==== Complexity
-- - \(O(n)\)
new :: (PrimMonad m, VU.Unbox cap, VU.Unbox cost) => Int -> m (McfGraph (PrimState m) cap cost)
new nG = do
  edgesG <- ACIGV.new 0
  pure McfGraph {..}

-- | Adds an edge oriented from @from@ to @to@ with capacity @cap@ and cost @cost@. It returns an
-- integer \(k\) such that this is the \(k\)-th edge that is added.
--
-- ==== Constraints
-- - \(0 \leq \mathrm{from}, \mathrm{to} \lt n\)
-- - \(0 \leq \mathrm{cap}, \mathrm{cost}\)
--
-- ==== Complexity
-- - \(O(1)\) amortized
addEdge ::
  (HasCallStack, PrimMonad m, Num cap, Ord cap, VU.Unbox cap, Num cost, Ord cost, VU.Unbox cost) =>
  McfGraph (PrimState m) cap cost ->
  Int ->
  Int ->
  cap ->
  cost ->
  m Int
addEdge McfGraph {..} from to cap cost = do
  let !_ = ACIA.checkCustom "AtCoder.MinCostFlow.addEdge" "`from` vertex" from "the number of vertices" nG
  let !_ = ACIA.checkCustom "AtCoder.MinCostFlow.addEdge" "`to` vertex" to "the number of vertices" nG
  let !_ = ACIA.runtimeAssert (0 <= cap) "AtCoder.MinCostFlow.addEdge: given invalid edge `cap` less than `0`"
  let !_ = ACIA.runtimeAssert (0 <= cost) "AtCoder.MinCostFlow.addEdge: given invalid edge `cost` less than `0`"
  m <- ACIGV.length edgesG
  ACIGV.pushBack edgesG (from, to, cap, 0, cost)
  pure m

-- | `addEdge` with return value discarded.
--
-- ==== Constraints
-- - \(0 \leq \mathrm{from}, \mathrm{to} \lt n\)
-- - \(0 \leq \mathrm{cap}, \mathrm{cost}\)
--
-- ==== Complexity
-- - \(O(1)\) amortized
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
  pure ()

-- | Augments the flow from \(s\) to \(t\) as much as possible, until reaching the amount of
-- @flowLimit@. It returns the amount of the flow and the cost.
--
-- ==== Constraints
-- - Same as `slope`.
--
-- ==== Complexity
-- - Same as `slope`.
flow ::
  (HasCallStack, PrimMonad m, Integral cap, Ord cap, VU.Unbox cap, Num cost, Ord cost, Bounded cost, VU.Unbox cost) =>
  McfGraph (PrimState m) cap cost ->
  Int ->
  Int ->
  cap ->
  m (cap, cost)
flow graph s t flowLimit = do
  res <- slope graph s t flowLimit
  pure $ VG.last res

-- | `flow` with no capacity limit.
--
-- ==== Constraints
-- - Same as `slope`.
--
-- ==== Complexity
-- - Same as `slope`.
maxFlow ::
  (HasCallStack, PrimMonad m, Integral cap, Ord cap, Bounded cap, VU.Unbox cap, Num cost, Ord cost, Bounded cost, VU.Unbox cost) =>
  McfGraph (PrimState m) cap cost ->
  Int ->
  Int ->
  m (cap, cost)
maxFlow graph s t = do
  res <- slope graph s t maxBound
  pure $ VG.last res

-- | Let \(g\) be a function such that \(g(x)\) is the cost of the minimum cost \(s-t\) flow when
-- the amount of the flow is exactly \(x\). \(g\) is known to be piecewise linear.
--
-- - The first element of the list is \((0, 0)\).
-- - Both of first and second tuple elements are strictly increasing.
-- - No three changepoints are on the same line.
-- - The last element of the list is \(y, g(y))\), where \(y = \min(x, \mathrm{flowLimit})\).
--
-- ==== Constraints
--
-- Let \(x\) be the maximum cost among all edges.
--
-- - \(s \neq t\)
-- - \(0 \leq s, t \lt n\)
-- - You can't call `slope`, `flow` or `maxFlow` multiple times.
-- - The total amount of the flow is in @cap@.
-- - The total cost of the flow is in @cost@.
-- - \(0 \leq nx \leq 8 \times 10^{18} + 1000\)
--
-- ==== Complexity
-- - \(O(F (n + m) \log (n + m))\), where \(F\) is the amount of the flow and \(m\) is the number of added edges.
slope ::
  (HasCallStack, PrimMonad m, Integral cap, Ord cap, VU.Unbox cap, Num cost, Ord cost, Bounded cost, VU.Unbox cost) =>
  McfGraph (PrimState m) cap cost ->
  Int ->
  Int ->
  cap ->
  m (VU.Vector (cap, cost))
slope McfGraph {..} s t flowLimit = do
  let !_ = ACIA.checkCustom "AtCoder.MinCostFlow.slope" "`source` vertex" s "the number of vertices" nG
  let !_ = ACIA.checkCustom "AtCoder.MinCostFlow.slope" "`sink` vertex" t "the number of vertices" nG
  let !_ = ACIA.runtimeAssert (s /= t) "AtCoder.MinCostFlow.slope: `source` and `sink` vertex have to be distict"

  edges_@(VU.V_5 _ _ _ caps _ _) <- ACIGV.unsafeFreeze edgesG
  (!edgeIdx, !g) <- ACIMCSR.build nG edges_
  result <- internalSlopeMCF g nG s t flowLimit

  (VUM.MV_5 _ _ _ _ flows _) <- readMutVar $ ACIGV.vecGV edgesG
  VU.iforM_ (VU.zip caps edgeIdx) $ \v (!cap1, !iEdge) -> do
    cap2 <- VGM.read (ACIMCSR.capCsr g) iEdge
    VGM.write flows v $! cap1 - cap2

  pure result

internalSlopeMCF ::
  forall cap cost m.
  (HasCallStack, PrimMonad m, Integral cap, Ord cap, VU.Unbox cap, Num cost, Ord cost, Bounded cost, VU.Unbox cost) =>
  ACIMCSR.Csr (PrimState m) cap cost ->
  Int ->
  Int ->
  Int ->
  cap ->
  m (VU.Vector (cap, cost))
internalSlopeMCF csr@ACIMCSR.Csr {..} n s t flowLimit = do
  duals <- VUM.replicate n 0
  dists <- VUM.unsafeNew n :: m (VUM.MVector (PrimState m) cost)
  prevE <- VUM.unsafeNew n :: m (VUM.MVector (PrimState m) Int)
  vis <- VUM.unsafeNew n :: m (VUM.MVector (PrimState m) Bit)

  -- FIXME: maximum capacity?
  let nEdges = VU.length toCsr
  queMin <- ACIB.new nEdges :: m (ACIB.Buffer (PrimState m) Int)
  heap <- ACIMH.new nEdges :: m (ACIMH.Heap (PrimState m) (cost, Int))

  let dualRef = do
        VGM.set dists $ maxBound @cost
        VGM.set vis $ Bit False
        ACIB.clear queMin
        -- TODO: compare with the first element only, so make up custom Q data type
        ACIMH.clear heap

        VGM.write dists s 0
        ACIB.pushBack queMin s
        fix $ \loop -> do
          b1 <- ACIB.null queMin
          b2 <- ACIMH.null heap
          when (not b1 || not b2) $ do
            v <-
              if not b1
                then do
                  fromJust <$> ACIB.popBack queMin
                else do
                  (!_, !to) <- fromJust <$> ACIMH.pop heap
                  pure to

            Bit visV <- VGM.exchange vis v $ Bit True
            unless (v == t) $ do
              unless visV $ do
                -- dist[v] = shortest(s, v) + dual[s] - dual[v]
                -- dist[v] >= 0 (all reduced cost are positive)
                -- dist[v] <= (n-1)C
                dualV <- VGM.read duals v
                distV <- VGM.read dists v
                let start = startCsr VG.! v
                VU.iforM_ (ACIMCSR.adj csr v) $ \di (!to, !rev, !cost) -> do
                  cap <- VGM.read capCsr $ start + di

                  unless (cap == 0) $ do
                    -- \|-dual[e.to] + dual[v]| <= (n-1)C
                    -- cost <= C - -(n-1)C + 0 = nC
                    cost' <- do
                      dualTo <- VGM.read duals to
                      pure $! cost - dualTo + dualV
                    distTo <- VGM.read dists to
                    when (distTo - distV > cost') $ do
                      let !distTo' = distV + cost'
                      VGM.write dists to distTo'
                      VGM.write prevE to rev
                      if distTo' == distV
                        then ACIB.pushBack queMin to
                        else ACIMH.push heap (distTo', to)

              loop
        Bit visT <- VGM.read vis t
        when visT $ do
          distT <- VGM.read dists t
          vis' <- VU.unsafeFreeze vis
          dists' <- VU.unsafeFreeze dists
          VU.iforM_ (VU.zip vis' dists') $ \v (Bit !visV, !distV) -> do
            when visV $ do
              VGM.modify duals (subtract (distT - distV)) v
        pure visT

  result <- ACIGV.new 16
  ACIGV.pushBack result (0 :: cap, 0 :: cost)

  let inner :: cap -> cost -> cost -> m ()
      inner flow_ cost prevCostPerFlow =
        when (flow_ < flowLimit) $ do
          b <- dualRef
          when b $ do
            prevE' <- VU.unsafeFreeze prevE

            let minC :: cap -> Int -> m cap
                minC !acc v
                  | v == s = pure acc
                  | otherwise = do
                      let iPrev = prevE' VG.! v
                      cap <- VGM.read capCsr $ revCsr VG.! iPrev
                      minC (min acc cap) $ toCsr VG.! iPrev
            c <- minC (flowLimit - flow_) t

            let subC :: Int -> m ()
                subC v = when (v /= s) $ do
                  let iPrev = prevE' VG.! v
                  VGM.modify capCsr (+ c) iPrev
                  VGM.modify capCsr (subtract c) $ revCsr VG.! iPrev
                  subC $ toCsr VG.! iPrev
            subC t

            d <- negate <$> VGM.read duals s
            let !flow' = flow_ + c
            let !cost' = cost + fromIntegral c * d -- TODO: minimize the type boundary
            when (prevCostPerFlow == d) $ do
              ACIGV.popBack_ result
            ACIGV.pushBack result (flow', cost')
            inner flow' cost' d

  inner 0 0 (-1)
  ACIGV.unsafeFreeze result

-- | Returns the current internal state of the edges: @(from, to, cap, flow, cost)@. The edges are
-- ordered in the same order as added by `addEdge`.
--
-- ==== Constraints
-- - \(0 \leq i \lt m\)
--
-- ==== Complexity
-- - \(O(1)\)
getEdge ::
  (HasCallStack, PrimMonad m, Num cap, Ord cap, VU.Unbox cap, Num cost, Ord cost, VU.Unbox cost) =>
  McfGraph (PrimState m) cap cost ->
  Int ->
  m (Int, Int, cap, cap, cost)
getEdge McfGraph {..} i = do
  m <- ACIGV.length edgesG
  let !_ = ACIA.checkEdge "AtCoder.MinCostFlow.getEdge" i m
  ACIGV.read edgesG i

-- | Returns the current internal state of the edges: @(from, to, cap, flow, cost)@. The edges are
-- ordered in the same order as added by `addEdge`.
--
-- ==== Complexity
-- - \(O(m)\), where \(m\) is the number of added edges.
edges ::
  (HasCallStack, PrimMonad m, Num cap, Ord cap, VU.Unbox cap, Num cost, Ord cost, VU.Unbox cost) =>
  McfGraph (PrimState m) cap cost ->
  m (VU.Vector (Int, Int, cap, cap, cost))
edges McfGraph {..} = do
  ACIGV.freeze edgesG

-- | Returns the current internal state of the edges: @(from, to, cap, flow, cost)@, but without
-- making copy. The edges are ordered in the same order as added by `addEdge`.
--
-- ==== Complexity
-- - \(O(1)\)
unsafeEdges ::
  (HasCallStack, PrimMonad m, Num cap, Ord cap, VU.Unbox cap, Num cost, Ord cost, VU.Unbox cost) =>
  McfGraph (PrimState m) cap cost ->
  m (VU.Vector (Int, Int, cap, cap, cost))
unsafeEdges McfGraph {..} = do
  ACIGV.unsafeFreeze edgesG
