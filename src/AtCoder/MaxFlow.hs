{-# LANGUAGE RecordWildCards #-}

-- | Maximum flow. The API is different from the original ACL.
--
-- The graph cannot be modified after run
module AtCoder.MaxFlow (Builder, new, new', addEdge, build, Graph, getEdge, edges, flow, flow') where

import AtCoder.Internal.Assert
import AtCoder.Internal.Buffer qualified as ACB
import AtCoder.Internal.Queue qualified as ACQ
import Control.Monad (when)
import Control.Monad.Extra (whenJustM)
import Control.Monad.Fix (fix)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Vector qualified as V
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Stack (HasCallStack)

-- TODO: combine builder and the max flow
-- TODO: re-consider the capacity type constraints

-- | Max flow graph builder.
data Builder s cap = Builder
  { -- | The number of vertices.
    nB :: {-# UNPACK #-} !Int,
    -- | Out degrees.
    degB :: !(VUM.MVector s Int),
    -- | Forward edges.
    esB :: !(ACB.Buffer s (Int, Int, cap))
  }

-- | \(O(n^2)\) Creates a `Builder` of the max flow `Graph`.
new :: (PrimMonad m, VU.Unbox cap) => Int -> m (Builder (PrimState m) cap)
new nB = do
  -- FIXME: Better edge size assumption.
  new' nB (nB * nB + nB)

-- | \(O(n + e)\)
new' :: (PrimMonad m, VU.Unbox cap) => Int -> Int -> m (Builder (PrimState m) cap)
new' nB nEdges = do
  degB <- VUM.replicate nB 0
  esB <- ACB.new nEdges
  return Builder {..}

-- | \(O(1)\)
addEdge :: (HasCallStack, PrimMonad m, Num cap, Ord cap, VU.Unbox cap) => Builder (PrimState m) cap -> Int -> Int -> cap -> m ()
addEdge Builder {..} from to cap = do
  let !_ = runtimeAssert (0 <= from && from < nB) "from vertex out of bounds"
  let !_ = runtimeAssert (0 <= to && to < nB) "to vertex out of bounds"
  let !_ = runtimeAssert (0 <= cap) "capacity has to bigger than or equal to 0"
  VGM.modify degB (+ 1) from
  VGM.modify degB (+ 1) to -- reverse edge
  ACB.pushBack esB (from, to, cap)

-- | \(O(n)\) Creates a maximum from `Graph` from a `Builder`. NOTE: The `Builder` is consumed and
-- cannot be reused.
build :: (HasCallStack, PrimMonad m, Num cap, Ord cap, VU.Unbox cap) => Builder (PrimState m) cap -> m (Graph (PrimState m) cap)
build Builder {..} = do
  gG <- V.mapM VUM.unsafeNew . VU.convert =<< VU.unsafeFreeze degB
  es <- ACB.unsafeFreeze esB
  posG <- VU.forM es $ \(!from, !to, !cap) -> do
    -- FIXME: copy degB? (on builder removeal)
    VGM.modify degB (subtract 1) from
    iEdge <- VGM.read degB from
    VGM.modify degB (subtract 1) to
    iRevEdge <- VGM.read degB to
    VGM.write (gG VG.! from) iEdge (to, iRevEdge, cap)
    VGM.write (gG VG.! to) iRevEdge (from, iEdge, 0)
    -- remember the forward edge information:
    return (from, iEdge)
  return Graph {nG = nB, ..}

-- | Max flow graph.
data Graph s cap = Graph
  { -- | The number of vertices.
    nG :: {-# UNPACK #-} !Int,
    -- | Graph: fromVertex -> vector of @(toVertex, revEdgeIndex, capacity)@.
    gG :: !(V.Vector (VUM.MVector s (Int, Int, cap))),
    -- | Forward edge information: originalEdgeIndex -> (fromVertex, edgeIndex)
    posG :: !(VU.Vector (Int, Int))
  }

-- | \(O(1)\) Retrieves ith edge: @(from, to, capacity, flow)@.
getEdge :: (PrimMonad m, Num cap, Ord cap, VU.Unbox cap) => Graph (PrimState m) cap -> Int -> m (Int, Int, cap, cap)
getEdge Graph {..} i = do
  let !_ = runtimeAssert (0 <= i && i < VU.length posG) "edge index out of bounds"
  let (!from, !iEdge) = posG VG.! i
  (!to, !iRevEdge, !cap) <- VGM.read (gG VG.! from) iEdge
  revCap <- VGM.read ((\(VUM.MV_3 _ _ _ c) -> c) (gG VG.! to)) iRevEdge
  return (from, to, cap + revCap, revCap)

-- | \(O(1)\) Retrieves all the edges: @(from, to, capacity, flow)@.
edges :: (PrimMonad m, Num cap, Ord cap, VU.Unbox cap) => Graph (PrimState m) cap -> m (VU.Vector (Int, Int, cap, cap))
edges g = VU.generateM (VU.length (posG g)) (getEdge g)

-- TODO: changeEdge

-- | \(O(n^2m)\) (m: the number of edges)
flow :: (HasCallStack, Show cap, PrimMonad m, Bounded cap, Num cap, Ord cap, VU.Unbox cap) => Graph (PrimState m) cap -> Int -> Int -> m cap
flow gr s t = do
  flow' gr s t maxBound

flow' :: (HasCallStack, Show cap, PrimMonad m, Num cap, Ord cap, VU.Unbox cap) => Graph (PrimState m) cap -> Int -> Int -> cap -> m cap
flow' Graph {..} s t flowLimit = do
  let !_ = runtimeAssert (0 <= s && s < nG) "source vertex out of bounds"
  let !_ = runtimeAssert (0 <= t && t < nG) "destination vertex out of bounds"
  let !_ = runtimeAssert (s /= t) "source and destination vertex has to be distinct"

  level <- VUM.unsafeNew nG
  que <- ACQ.new nG
  let bfs = do
        VGM.set level (-1 :: Int)
        VGM.write level s 0
        ACQ.clear que
        ACQ.pushBack que s
        fix $ \loop -> do
          whenJustM (ACQ.popFront que) $ \v -> do
            let (VUM.MV_3 _ vecTo _ vecCap) = gG VG.! v
            neighbors <- VU.zip <$> VU.unsafeFreeze vecTo <*> VU.unsafeFreeze vecCap
            VU.forM_ neighbors $ \(!to, !cap) -> do
              when (cap /= 0) $ do
                levelTo <- VGM.read level to
                when (levelTo < 0) $ do
                  levelV <- VGM.read level v
                  VGM.write level to (levelV + 1)
                  -- FIXME: break on to == t
                  ACQ.pushBack que to
            levelT <- VGM.read level t
            when (levelT == -1) $ do
              loop

  iter <- VUM.unsafeNew nG
  let dfs v up
        | v == s = return up
        | otherwise = do
            let len = VUM.length (gG VG.! v)
            levelV <- VGM.read level v
            result <- flip fix 0 $ \loop res -> do
              i <- VGM.read iter v
              if i >= len
                then return res
                else do
                  VGM.write iter v $ i + 1
                  (!to, !iRevEdge, !_) <- VGM.read (gG VG.! v) i
                  levelTo <- VGM.read level to
                  revCap <- VGM.read ((\(VUM.MV_3 _ _ _ c) -> c) (gG VG.! to)) iRevEdge
                  if levelV <= levelTo || revCap == 0
                    then loop res
                    else do
                      d <- dfs to $! min (up - res) revCap
                      if d <= 0
                        then loop res -- no flow. ignore
                        else do
                          VGM.modify ((\(VUM.MV_3 _ _ _ c) -> c) (gG VG.! v)) (+ d) i
                          VGM.modify ((\(VUM.MV_3 _ _ _ c) -> c) (gG VG.! to)) (subtract d) iRevEdge
                          let !res' = res + d
                          if res' == up
                            then return res'
                            else loop res' -- next neighbor
            VGM.write level v nG
            return result

  flip fix 0 $ \loop flow_ -> do
    if flow_ >= flowLimit
      then return flow_
      else do
        bfs
        levelT <- VGM.read level t
        if levelT == -1
          then return flow_
          else do
            VGM.set iter (0 :: Int)
            f <- dfs t $! flowLimit - flow_
            if f == 0
              then return flow_
              else loop $! flow_ + f

-- TODO: minCut
