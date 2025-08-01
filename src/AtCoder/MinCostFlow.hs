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
--
-- >>> MCF.addEdge_ g 1 2 2 5   --  0 --> 1 --> 2
--
-- Augment flow with `flow`, `maxFlow` or `slope`:
--
-- >>> MCF.slope g 0 2 maxBound -- slope g from to flowLimit
-- [(0,0),(2,16)]
--
-- Note that you can't call `flow`, `maxFlow` or `slope` multiple times, or else you'll get wrong
-- return value.
--
-- @since 1.0.0.0
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
-- TODO: is this fast enough with `INLINEABLE`?

import AtCoder.Internal.Assert qualified as ACIA
import AtCoder.Internal.Buffer qualified as ACIB
import AtCoder.Internal.GrowVec qualified as ACIGV
import AtCoder.Internal.McfCsr qualified as ACIMCSR
import AtCoder.Internal.MinHeap qualified as ACIMH
import Control.Monad (unless, when)
import Control.Monad.Fix (fix)
import Control.Monad.Primitive (PrimMonad, PrimState, stToPrim)
import Control.Monad.ST (ST)
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
--
-- @since 1.0.0.0
data McfGraph s cap cost = McfGraph
  { -- | The number of vertices.
    --
    -- @since 1.0.0.0
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
--
-- @since 1.0.0.0
{-# INLINE new #-}
new :: (PrimMonad m, VU.Unbox cap, VU.Unbox cost) => Int -> m (McfGraph (PrimState m) cap cost)
new nG = stToPrim $ do
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
--
-- @since 1.0.0.0
{-# INLINE addEdge #-}
addEdge ::
  (HasCallStack, PrimMonad m, Num cap, Ord cap, VU.Unbox cap, Num cost, Ord cost, VU.Unbox cost) =>
  -- | Graph
  McfGraph (PrimState m) cap cost ->
  -- | from
  Int ->
  -- | to
  Int ->
  -- | capacity
  cap ->
  -- | cost
  cost ->
  -- | Edge index
  m Int
addEdge g from to cap cost = stToPrim $ addEdgeST g from to cap cost

-- | `addEdge` with the return value discarded.
--
-- ==== Constraints
-- - \(0 \leq \mathrm{from}, \mathrm{to} \lt n\)
-- - \(0 \leq \mathrm{cap}, \mathrm{cost}\)
--
-- ==== Complexity
-- - \(O(1)\) amortized
--
-- @since 1.0.0.0
{-# INLINE addEdge_ #-}
addEdge_ ::
  (HasCallStack, PrimMonad m, Num cap, Ord cap, VU.Unbox cap, Num cost, Ord cost, VU.Unbox cost) =>
  -- | Graph
  McfGraph (PrimState m) cap cost ->
  -- | from
  Int ->
  -- | to
  Int ->
  -- | capacity
  cap ->
  -- | cost
  cost ->
  m ()
addEdge_ graph from to cap cost = stToPrim $ do
  _ <- addEdgeST graph from to cap cost
  pure ()

-- | Augments the flow from \(s\) to \(t\) as much as possible, until reaching the amount of
-- @flowLimit@. It returns the amount of the flow and the cost.
--
-- ==== Constraints
-- - Same as `slope`.
--
-- ==== Complexity
-- - Same as `slope`.
--
-- @since 1.0.0.0
{-# INLINE flow #-}
flow ::
  (HasCallStack, PrimMonad m, Integral cap, Ord cap, VU.Unbox cap, Num cost, Ord cost, Bounded cost, VU.Unbox cost) =>
  -- | Graph
  McfGraph (PrimState m) cap cost ->
  -- | Source @s@
  Int ->
  -- | Sink @t@
  Int ->
  -- | Flow limit
  cap ->
  -- | Tuple of @(cap, cost@)
  m (cap, cost)
flow graph s t flowLimit = stToPrim $ do
  res <- slopeST graph s t flowLimit
  pure $ VG.last res

-- | `flow` with no capacity limit.
--
-- ==== Constraints
-- - Same as `slope`.
--
-- ==== Complexity
-- - Same as `slope`.
--
-- @since 1.0.0.0
{-# INLINE maxFlow #-}
maxFlow ::
  (HasCallStack, PrimMonad m, Integral cap, Ord cap, Bounded cap, VU.Unbox cap, Num cost, Ord cost, Bounded cost, VU.Unbox cost) =>
  -- | Graph
  McfGraph (PrimState m) cap cost ->
  -- | Source @s@
  Int ->
  -- | Sink @t@
  Int ->
  -- | Tuple of @(cap, cost@)
  m (cap, cost)
maxFlow graph s t = stToPrim $ do
  res <- slopeST graph s t maxBound
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
--
-- @since 1.0.0.0
{-# INLINE slope #-}
slope ::
  (HasCallStack, PrimMonad m, Integral cap, Ord cap, VU.Unbox cap, Num cost, Ord cost, Bounded cost, VU.Unbox cost) =>
  -- | Graph
  McfGraph (PrimState m) cap cost ->
  -- | Source @s@
  Int ->
  -- | Sink @t@
  Int ->
  -- | Flow limit
  cap ->
  -- | Vector of @(cap, cost)@
  m (VU.Vector (cap, cost))
slope g s t flowLimit = stToPrim $ slopeST g s t flowLimit

-- | Returns the current internal state of the edges: @(from, to, cap, flow, cost)@. The edges are
-- ordered in the same order as added by `addEdge`.
--
-- ==== Constraints
-- - \(0 \leq i \lt m\)
--
-- ==== Complexity
-- - \(O(1)\)
--
-- @since 1.0.0.0
{-# INLINE getEdge #-}
getEdge ::
  (HasCallStack, PrimMonad m, Num cap, Ord cap, VU.Unbox cap, Num cost, Ord cost, VU.Unbox cost) =>
  -- | Graph
  McfGraph (PrimState m) cap cost ->
  -- | Edge index
  Int ->
  -- | Tuple of @(from, to, cap, flow, cost)@
  m (Int, Int, cap, cap, cost)
getEdge g i = stToPrim $ getEdgeST g i

-- | Returns the current internal state of the edges: @(from, to, cap, flow, cost)@. The edges are
-- ordered in the same order as added by `addEdge`.
--
-- ==== Complexity
-- - \(O(m)\), where \(m\) is the number of added edges.
--
-- @since 1.0.0.0
{-# INLINE edges #-}
edges ::
  (HasCallStack, PrimMonad m, Num cap, Ord cap, VU.Unbox cap, Num cost, Ord cost, VU.Unbox cost) =>
  -- | Graph
  McfGraph (PrimState m) cap cost ->
  -- | Vector of @(from, to, cap, flow, cost)@
  m (VU.Vector (Int, Int, cap, cap, cost))
edges McfGraph {..} = stToPrim $ do
  ACIGV.freeze edgesG

-- | Returns the current internal state of the edges: @(from, to, cap, flow, cost)@, but without
-- making copy. The edges are ordered in the same order as added by `addEdge`.
--
-- ==== Complexity
-- - \(O(1)\)
--
-- @since 1.0.0.0
{-# INLINE unsafeEdges #-}
unsafeEdges ::
  (HasCallStack, PrimMonad m, Num cap, Ord cap, VU.Unbox cap, Num cost, Ord cost, VU.Unbox cost) =>
  -- | Graph
  McfGraph (PrimState m) cap cost ->
  -- | Vector of @(from, to, cap, flow, cost)@
  m (VU.Vector (Int, Int, cap, cap, cost))
unsafeEdges McfGraph {..} = stToPrim $ do
  ACIGV.unsafeFreeze edgesG

-- -------------------------------------------------------------------------------------------------
-- Internal
-- -------------------------------------------------------------------------------------------------

{-# INLINEABLE addEdgeST #-}
addEdgeST ::
  (HasCallStack, Num cap, Ord cap, VU.Unbox cap, Num cost, Ord cost, VU.Unbox cost) =>
  -- | Graph
  McfGraph s cap cost ->
  -- | from
  Int ->
  -- | to
  Int ->
  -- | capacity
  cap ->
  -- | cost
  cost ->
  -- | Edge index
  ST s Int
addEdgeST McfGraph {..} from to cap cost = do
  let !_ = ACIA.checkCustom "AtCoder.MinCostFlow.addEdgeST" "`from` vertex" from "the number of vertices" nG
  let !_ = ACIA.checkCustom "AtCoder.MinCostFlow.addEdgeST" "`to` vertex" to "the number of vertices" nG
  let !_ = ACIA.runtimeAssert (0 <= cap) "AtCoder.MinCostFlow.addEdgeST: given invalid edge `cap` less than `0`"
  let !_ = ACIA.runtimeAssert (0 <= cost) "AtCoder.MinCostFlow.addEdgeST: given invalid edge `cost` less than `0`"
  m <- ACIGV.length edgesG
  ACIGV.pushBack edgesG (from, to, cap, 0, cost)
  pure m

{-# INLINEABLE slopeST #-}
slopeST ::
  (HasCallStack, Integral cap, Ord cap, VU.Unbox cap, Num cost, Ord cost, Bounded cost, VU.Unbox cost) =>
  -- | Graph
  McfGraph s cap cost ->
  -- | Source @s@
  Int ->
  -- | Sink @t@
  Int ->
  -- | Flow limit
  cap ->
  -- | Vector of @(cap, cost)@
  ST s (VU.Vector (cap, cost))
slopeST McfGraph {..} s t flowLimit = do
  let !_ = ACIA.checkCustom "AtCoder.MinCostFlow.slopeST" "`source` vertex" s "the number of vertices" nG
  let !_ = ACIA.checkCustom "AtCoder.MinCostFlow.slopeST" "`sink` vertex" t "the number of vertices" nG
  let !_ = ACIA.runtimeAssert (s /= t) "AtCoder.MinCostFlow.slopeST: `source` and `sink` vertex must be distinct"

  edges_@(VU.V_5 _ _ _ caps _ _) <- ACIGV.unsafeFreeze edgesG
  (!edgeIdx, !g) <- ACIMCSR.build nG edges_
  result <- internalSlopeST g nG s t flowLimit

  (VUM.MV_5 _ _ _ _ flows _) <- readMutVar $ ACIGV.vecGV edgesG
  VU.iforM_ (VU.zip caps edgeIdx) $ \v (!cap1, !iEdge) -> do
    cap2 <- VGM.read (ACIMCSR.capCsr g) iEdge
    VGM.write flows v $! cap1 - cap2

  pure result

{-# INLINEABLE internalSlopeST #-}
internalSlopeST ::
  forall cap cost s.
  (HasCallStack, Integral cap, Ord cap, VU.Unbox cap, Num cost, Ord cost, Bounded cost, VU.Unbox cost) =>
  ACIMCSR.Csr s cap cost ->
  Int ->
  Int ->
  Int ->
  cap ->
  ST s (VU.Vector (cap, cost))
internalSlopeST csr@ACIMCSR.Csr {..} n s t flowLimit = do
  duals <- VUM.replicate n 0
  dists <- VUM.unsafeNew n :: ST s (VUM.MVector s cost)
  prevE <- VUM.unsafeNew n :: ST s (VUM.MVector s Int)
  vis <- VUM.unsafeNew n :: ST s (VUM.MVector s Bit)

  -- FIXME: maximum capacity of heap?
  let nEdges = VU.length toCsr
  queMin <- ACIB.new nEdges :: ST s (ACIB.Buffer s Int)
  heap <- ACIMH.new nEdges :: ST s (ACIMH.Heap s (cost, Int))

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
                    -- - |-dual[e.to] + dual[v]| <= (n-1)C
                    -- - cost <= C - -(n-1)C + 0 = nC
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

  let inner :: cap -> cost -> cost -> ST s ()
      inner flow_ cost prevCostPerFlow =
        when (flow_ < flowLimit) $ do
          b <- dualRef
          when b $ do
            prevE' <- VU.unsafeFreeze prevE

            let minC :: cap -> Int -> ST s cap
                minC !acc v
                  | v == s = pure acc
                  | otherwise = do
                      let iPrev = prevE' VG.! v
                      cap <- VGM.read capCsr $ revCsr VG.! iPrev
                      minC (min acc cap) $ toCsr VG.! iPrev
            c <- minC (flowLimit - flow_) t

            let subC :: Int -> ST s ()
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

{-# INLINEABLE getEdgeST #-}
getEdgeST ::
  (HasCallStack, Num cap, Ord cap, VU.Unbox cap, Num cost, Ord cost, VU.Unbox cost) =>
  -- | Graph
  McfGraph s cap cost ->
  -- | Edge index
  Int ->
  -- | Tuple of @(from, to, cap, flow, cost)@
  ST s (Int, Int, cap, cap, cost)
getEdgeST McfGraph {..} i = do
  m <- ACIGV.length edgesG
  let !_ = ACIA.checkEdge "AtCoder.MinCostFlow.getEdgeST" i m
  ACIGV.read edgesG i
