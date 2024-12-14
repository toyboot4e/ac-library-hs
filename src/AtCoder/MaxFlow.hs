{-# LANGUAGE RecordWildCards #-}

-- | It solves [maximum flow problem](https://en.wikipedia.org/wiki/Maximum_flow_problem).
--
-- ==== __Example__
-- Create a max flow graph (`MfGraph`):
--
-- >>> import AtCoder.MaxFlow qualified as MF
-- >>> g <- MF.new @_ @Int 3        --  0     1     2
--
-- Build a simple graph with `'addEdge' g from to cap` or `addEdge_`:
--
-- >>> MF.addEdge g 0 1 (2 :: Int)  --  0 --> 1     2
-- 0
-- >>> MF.addEdge_ g 1 2 (1 :: Int) --  0 --> 1 --> 2
--
-- Augument the flow with `flow`. `maxFlow` can also be used when there's no flow limit:
--
-- >>> MF.flow g 0 2 {- flowLimit -} maxBound -- same as `MF.maxFlow g 0 2`
-- 1
--
-- Get the minimum cut with `minCut`. In this case, removing the second edge makes the minimum cut
-- (note that the edge capacity (`1`) = max flow):
--
-- >>> MF.minCut g 0 -- returns a Bit vector. `1` (`Bit True`) is on the `s` side.
-- [1,1,0]
--
-- Retrieve the edge state with `getEdge`. We can confirm the flow is @1@:
--
-- >>> MF.getEdge g 0 -- returns (from, to, cap, flow)
-- (0,1,2,1)
--
-- @since 1.0.0
module AtCoder.MaxFlow
  ( -- * Max flow graph
    MfGraph (nG),

    -- * Constructor
    new,

    -- * Graph building
    addEdge,
    addEdge_,
    getEdge,

    -- * Flow operations
    flow,
    maxFlow,

    -- * Minimum cut
    minCut,

    -- * Edge information
    edges,
    changeEdge,
  )
where

-- TODO: add `build`.

import AtCoder.Internal.Assert qualified as ACIA
import AtCoder.Internal.GrowVec qualified as ACIGV
import AtCoder.Internal.Queue qualified as ACIQ
import Control.Monad (unless, when)
import Control.Monad.Fix (fix)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Bit (Bit (..))
import Data.Primitive.MutVar (readMutVar)
import Data.Vector qualified as V
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Stack (HasCallStack)

-- | Max flow graph.
--
-- @since 1.0.0
data MfGraph s cap = MfGraph
  { -- | The number of vertices.
    --
    -- @since 1.0.0
    nG :: {-# UNPACK #-} !Int,
    -- | MfGraph: fromVertex -> vector of @(toVertex, revEdgeIndex, capacity)@.
    gG :: !(V.Vector (ACIGV.GrowVec s (Int, Int, cap))),
    -- | Forward edge information: originalEdgeIndex -> (fromVertex, edgeIndex)
    posG :: !(ACIGV.GrowVec s (Int, Int))
  }

-- | Creates a graph of @n@ vertices and \(0\) edges. `cap` is the type of the capacity.
--
-- ==== Constraints
-- - \(0 \leq n\)
--
-- ==== Complexity
-- - \(O(n)\)
--
-- @since 1.0.0
new :: (PrimMonad m, VU.Unbox cap) => Int -> m (MfGraph (PrimState m) cap)
new nG = do
  gG <- V.replicateM nG (ACIGV.new 0)
  posG <- ACIGV.new 0
  pure MfGraph {..}

-- | Adds an edge oriented from the vertex @from@ to the vertex @to@ with the capacity @cap@ and the
-- flow amount \(0\). It returns an integer \(k\) such that this is the \(k\)-th edge that is added.
--
-- ==== Constraints
-- - \(0 \leq \mathrm{from}, \mathrm{to} \lt n\)
-- - \(0 \leq \mathrm{cap}\)
--
-- ==== Complexity
-- - \(O(1)\) amortized
--
-- @since 1.0.0
addEdge :: (HasCallStack, PrimMonad m, Num cap, Ord cap, VU.Unbox cap) => MfGraph (PrimState m) cap -> Int -> Int -> cap -> m Int
addEdge MfGraph {..} from to cap = do
  let !_ = ACIA.checkCustom "AtCoder.MaxFlow.addEdge" "`from` vertex" from "the number of vertices" nG
  let !_ = ACIA.checkCustom "AtCoder.MaxFlow.addEdge" "`to` vertex" to "the number of vertices" nG
  let !_ = ACIA.runtimeAssert (0 <= cap) "AtCoder.MaxFlow.addEdge: given invalid edge `cap` less than `0`" -- not `Show cap`
  m <- ACIGV.length posG
  iEdge <- ACIGV.length (gG VG.! from)
  ACIGV.pushBack posG (from, iEdge)
  iRevEdge <- do
    len <- ACIGV.length (gG VG.! to)
    pure $ if from == to then len + 1 else len
  ACIGV.pushBack (gG VG.! from) (to, iRevEdge, cap)
  ACIGV.pushBack (gG VG.! to) (from, iEdge, 0)
  pure m

-- | `addEdge` with return value discarded.
--
-- ==== Constraints
-- - \(0 \leq \mathrm{from}, \mathrm{to} \lt n\)
-- - \(0 \leq \mathrm{cap}\)
--
-- ==== Complexity
-- - \(O(1)\) amortized
--
-- @since 1.0.0
addEdge_ :: (HasCallStack, PrimMonad m, Num cap, Ord cap, VU.Unbox cap) => MfGraph (PrimState m) cap -> Int -> Int -> cap -> m ()
addEdge_ graph from to cap = do
  _ <- addEdge graph from to cap
  pure ()

-- | Augments the flow from \(s\) to \(t\) as much as possible, until reaching the amount of
-- @flowLimit@. It returns the amount of the flow augmented. You may call it multiple times.
--
-- ==== Constraints
-- - \(s \neq t\)
-- - \(0 \leq s, t \lt n\)
--
-- ==== Complexity
-- - \(O((n + m) \sqrt{m})\) (if all the capacities are \(1\)),
-- - \(O(n^2 m)\) (general), or
-- - \(O(F(n + m))\), where \(F\) is the returned value
--
-- @since 1.0.0
flow :: (HasCallStack, PrimMonad m, Num cap, Ord cap, VU.Unbox cap) => MfGraph (PrimState m) cap -> Int -> Int -> cap -> m cap
flow MfGraph {..} s t flowLimit = do
  let !_ = ACIA.checkCustom "AtCoder.MaxFlow.flow" "`source` vertex" s "the number of vertices" nG
  let !_ = ACIA.checkCustom "AtCoder.MaxFlow.flow" "`sink` vertex" t "the number of vertices" nG
  let !_ = ACIA.runtimeAssert (s /= t) $ "AtCoder.MaxFlow.flow: `source` and `sink` vertex have to be distinct: `" ++ show s ++ "`"

  level <- VUM.unsafeNew nG
  que <- ACIQ.new nG
  let bfs = do
        VGM.set level (-1 :: Int)
        VGM.write level s 0
        ACIQ.clear que
        ACIQ.pushBack que s
        fix $ \loop -> do
          v_ <- ACIQ.popFront que
          case v_ of
            Nothing -> pure ()
            Just v -> do
              (VUM.MV_3 _ vecTo _ vecCap) <- readMutVar $ ACIGV.vecGV (gG VG.! v)
              len <- ACIGV.length (gG VG.! v)
              neighbors <- VU.zip <$> VU.unsafeFreeze (VUM.take len vecTo) <*> VU.unsafeFreeze (VUM.take len vecCap)
              VU.forM_ neighbors $ \(!to, !cap) -> do
                when (cap /= 0) $ do
                  levelTo <- VGM.read level to
                  when (levelTo < 0) $ do
                    levelV <- VGM.read level v
                    VGM.write level to (levelV + 1)
                    -- FIXME: break on to == t
                    ACIQ.pushBack que to
              levelT <- VGM.read level t
              when (levelT == -1) $ do
                loop

  iter <- VUM.unsafeNew nG
  let dfs v up
        | v == s = pure up
        | otherwise = do
            len <- ACIGV.length (gG VG.! v)
            levelV <- VGM.read level v
            result <- flip fix 0 $ \loop res -> do
              i <- VGM.read iter v
              if i >= len
                then pure res
                else do
                  VGM.write iter v $ i + 1
                  (!to, !iRevEdge, !_) <- ACIGV.read (gG VG.! v) i
                  levelTo <- VGM.read level to
                  revCap <- readCapacity gG to iRevEdge
                  if levelV <= levelTo || revCap == 0
                    then loop res
                    else do
                      d <- dfs to $! min (up - res) revCap
                      if d <= 0
                        then loop res -- no flow. ignore
                        else do
                          modifyCapacity (gG VG.! v) (+ d) i
                          modifyCapacity (gG VG.! to) (subtract d) iRevEdge
                          let !res' = res + d
                          if res' == up
                            then pure res'
                            else loop res' -- next neighbor
            VGM.write level v nG
            pure result

  flip fix 0 $ \loop flow_ -> do
    if flow_ >= flowLimit
      then pure flow_
      else do
        bfs
        levelT <- VGM.read level t
        if levelT == -1
          then pure flow_
          else do
            VGM.set iter (0 :: Int)
            f <- dfs t $! flowLimit - flow_
            if f == 0
              then pure flow_
              else loop $! flow_ + f

-- | `flow` with no capacity limit.
--
-- ==== Constraints
-- - \(s \neq t\)
-- - \(0 \leq s, t \lt n\)
--
-- ==== Complexity
-- - \(O((n + m) \sqrt{m})\) (if all the capacities are \(1\)),
-- - \(O(n^2 m)\) (general), or
-- - \(O(F(n + m))\), where \(F\) is the returned value
--
-- @since 1.0.0
maxFlow :: (HasCallStack, PrimMonad m, Num cap, Ord cap, Bounded cap, VU.Unbox cap) => MfGraph (PrimState m) cap -> Int -> Int -> m cap
maxFlow graph s t = flow graph s t maxBound

-- | Returns a vector of length \(n\), such that the \(i\)-th element is `True` if and only if there
-- is a directed path from \(s\) to \(i\) in the residual network. The returned vector corresponds
-- to a \(s-t\) minimum cut after calling @'maxFlow' s t@.
--
-- ==== Complexity
-- - \(O(n + m)\), where \(m\) is the number of added edges.
--
-- @since 1.0.0
minCut :: (PrimMonad m, Num cap, Ord cap, VU.Unbox cap) => MfGraph (PrimState m) cap -> Int -> m (VU.Vector Bit)
minCut MfGraph {..} s = do
  visited <- VUM.replicate nG $ Bit False
  que <- ACIQ.new nG -- we could use a growable queue here
  ACIQ.pushBack que s
  fix $ \loop -> do
    p_ <- ACIQ.popFront que
    case p_ of
      Nothing -> pure ()
      Just p -> do
        VGM.write visited p $ Bit True
        es <- ACIGV.unsafeFreeze (gG VG.! p)
        VU.forM_ es $ \(!to, !_, !cap) -> do
          when (cap /= 0) $ do
            Bit b <- VGM.exchange visited to $ Bit True
            unless b $ do
              ACIQ.pushBack que to
        loop
  VU.unsafeFreeze visited

-- | \(O(1)\) Returns the current internal state of \(i\)-th edge: @(from, to, cap, flow)@. The
-- edges are ordered in the same order as added by `addEdge`.
--
-- ==== Constraints
-- - \(0 \leq i \lt m\)
--
-- ==== Complexity
-- - \(O(1)\)
--
-- @since 1.0.0
getEdge :: (HasCallStack, PrimMonad m, Num cap, Ord cap, VU.Unbox cap) => MfGraph (PrimState m) cap -> Int -> m (Int, Int, cap, cap)
getEdge MfGraph {..} i = do
  m <- ACIGV.length posG
  let !_ = ACIA.checkEdge "AtCoder.MaxFlow.getEdge" i m
  (!from, !iEdge) <- ACIGV.read posG i
  (!to, !iRevEdge, !cap) <- ACIGV.read (gG VG.! from) iEdge
  revCap <- readCapacity gG to iRevEdge
  pure (from, to, cap + revCap, revCap)

-- | Returns the current internal state of the edges: @(from, to, cap, flow)@. The edges are ordered
-- in the same order as added by `addEdge`.
--
-- ==== Complexity
-- - \(O(m)\), where \(m\) is the number of added edges.
--
-- @since 1.0.0
edges :: (PrimMonad m, Num cap, Ord cap, VU.Unbox cap) => MfGraph (PrimState m) cap -> m (VU.Vector (Int, Int, cap, cap))
edges g@MfGraph {posG} = do
  len <- ACIGV.length posG
  -- TODO: rewrite
  VU.generateM len (getEdge g)

-- | \(O(1)\) Changes the capacity and the flow amount of the $i$-th edge to @newCap@ and
-- @newFlow@, respectively. It doesn't change the capacity or the flow amount of other edges.
--
-- ==== Constraints
-- - \(0 \leq \mathrm{newflow} \leq \mathrm{newcap}\)
--
-- ==== Complexity
-- - \(O(1)\)
--
-- @since 1.0.0
changeEdge :: (HasCallStack, PrimMonad m, Num cap, Ord cap, VU.Unbox cap) => MfGraph (PrimState m) cap -> Int -> cap -> cap -> m ()
changeEdge MfGraph {..} i newCap newFlow = do
  m <- ACIGV.length posG
  let !_ = ACIA.checkEdge "AtCoder.MaxFlow.changeEdge" i m
  let !_ = ACIA.runtimeAssert (0 <= newFlow && newFlow <= newCap) "AtCoder.MaxFlow.changeEdge: invalid flow or capacity" -- not Show
  (!from, !iEdge) <- ACIGV.read posG i
  (!to, !iRevEdge, !_) <- ACIGV.read (gG VG.! from) iEdge
  writeCapacity gG from iEdge $! newCap - newFlow
  writeCapacity gG to iRevEdge $! newFlow

-- | \(O(1)\) Internal helper.
{-# INLINE readCapacity #-}
readCapacity :: (HasCallStack, PrimMonad m, Num cap, Ord cap, VU.Unbox cap) => V.Vector (ACIGV.GrowVec (PrimState m) (Int, Int, cap)) -> Int -> Int -> m cap
readCapacity gvs v i = do
  (VUM.MV_3 _ _ _ c) <- readMutVar $ ACIGV.vecGV $ gvs VG.! v
  VGM.read c i

-- | \(O(1)\) Internal helper.
{-# INLINE writeCapacity #-}
writeCapacity :: (HasCallStack, PrimMonad m, Num cap, Ord cap, VU.Unbox cap) => V.Vector (ACIGV.GrowVec (PrimState m) (Int, Int, cap)) -> Int -> Int -> cap -> m ()
writeCapacity gvs v i cap = do
  (VUM.MV_3 _ _ _ c) <- readMutVar $ ACIGV.vecGV $ gvs VG.! v
  VGM.write c i cap

-- | \(O(1)\) Internal helper.
{-# INLINE modifyCapacity #-}
modifyCapacity :: (HasCallStack, PrimMonad m, Num cap, Ord cap, VU.Unbox cap) => ACIGV.GrowVec (PrimState m) (Int, Int, cap) -> (cap -> cap) -> Int -> m ()
modifyCapacity gv f i = do
  (VUM.MV_3 _ _ _ c) <- readMutVar $ ACIGV.vecGV gv
  VUM.modify c f i
