{-# LANGUAGE RecordWildCards #-}

-- | Maximum flow.
module AtCoder.MaxFlow (MfGraph, new, new', addEdge, addEdge_, getEdge, edges, changeEdge, flow, flow', minCut) where

import AtCoder.Internal.Assert
import AtCoder.Internal.GrowVec qualified as ACGV
import AtCoder.Internal.Queue qualified as ACQ
import Control.Monad (unless, when)
import Control.Monad.Extra (whenJustM)
import Control.Monad.Fix (fix)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Primitive.MutVar (readMutVar)
import Data.Vector qualified as V
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Stack (HasCallStack)

-- | Max flow graph.
data MfGraph s cap = MfGraph
  { -- | The number of vertices.
    nG :: {-# UNPACK #-} !Int,
    -- | MfGraph: fromVertex -> vector of @(toVertex, revEdgeIndex, capacity)@.
    gG :: !(V.Vector (ACGV.GrowVec s (Int, Int, cap))),
    -- | Forward edge information: originalEdgeIndex -> (fromVertex, edgeIndex)
    posG :: !(ACGV.GrowVec s (Int, Int))
  }

-- | \(O(n^2)\)
new :: (VU.Unbox cap, PrimMonad m) => Int -> m (MfGraph (PrimState m) cap)
new nG = do
  new' nG 0

-- | \(O(n + e)\)
new' :: (VU.Unbox cap, PrimMonad m) => Int -> Int -> m (MfGraph (PrimState m) cap)
new' nG nEdges = do
  gG <- V.replicateM nG (ACGV.new 0)
  posG <- ACGV.new nEdges
  return MfGraph {..}

-- | Amortized \(O(1)\)
addEdge :: (HasCallStack, PrimMonad m, Num cap, Ord cap, VU.Unbox cap) => MfGraph (PrimState m) cap -> Int -> Int -> cap -> m Int
addEdge MfGraph {..} from to cap = do
  let !_ = runtimeAssert (0 <= from && from < nG) "from vertex out of bounds"
  let !_ = runtimeAssert (0 <= to && to < nG) "to vertex out of bounds"
  let !_ = runtimeAssert (0 <= cap) "capacity has to bigger than or equal to 0"
  m <- ACGV.length posG
  iEdge <- ACGV.length (gG VG.! from)
  ACGV.pushBack posG (from, iEdge)
  iRevEdge <- do
    len <- ACGV.length (gG VG.! to)
    return $ if from == to then len + 1 else len
  ACGV.pushBack (gG VG.! from) (to, iRevEdge, cap)
  ACGV.pushBack (gG VG.! to) (from, iEdge, 0)
  return m

-- | Amortized \(O(1)\)
--
-- This function is not in the original ac-library.
addEdge_ :: (HasCallStack, PrimMonad m, Num cap, Ord cap, VU.Unbox cap) => MfGraph (PrimState m) cap -> Int -> Int -> cap -> m ()
addEdge_ graph from to cap = do
  _ <- addEdge graph from to cap
  return ()

-- | \(O(1)\) Wrie edge capactiy and flow.
changeEdge :: (HasCallStack, PrimMonad m, Num cap, Ord cap, VU.Unbox cap) => MfGraph (PrimState m) cap -> Int -> cap -> cap -> m ()
changeEdge MfGraph {..} i newCap newFlow = do
  m <- ACGV.length posG
  let !_ = runtimeAssert (0 <= i && i < m) "changeEdge: vertex out of bounds"
  let !_ = runtimeAssert (0 <= newFlow && newFlow <= newCap) "changeEdge: invalid flow and capacity"
  (!from, !iEdge) <- ACGV.read posG i
  (!to, !iRevEdge, !_) <- ACGV.read (gG VG.! from) iEdge
  writeCapacity gG from iEdge $! newCap - newFlow
  writeCapacity gG to iRevEdge $! newFlow

-- | \(O(1)\) Internal helper.
{-# INLINE readCapacity #-}
readCapacity :: (HasCallStack, PrimMonad m, Num cap, Ord cap, VU.Unbox cap) => V.Vector (ACGV.GrowVec (PrimState m) (Int, Int, cap)) -> Int -> Int -> m cap
readCapacity gvs v i = do
  (VUM.MV_3 _ _ _ c) <- readMutVar $ ACGV.vecGV $ gvs VG.! v
  VGM.read c i

-- | \(O(1)\) Internal helper.
{-# INLINE writeCapacity #-}
writeCapacity :: (HasCallStack, PrimMonad m, Num cap, Ord cap, VU.Unbox cap) => V.Vector (ACGV.GrowVec (PrimState m) (Int, Int, cap)) -> Int -> Int -> cap -> m ()
writeCapacity gvs v i cap = do
  (VUM.MV_3 _ _ _ c) <- readMutVar $ ACGV.vecGV $ gvs VG.! v
  VGM.write c i cap

-- | \(O(1)\) Internal helper.
{-# INLINE modifyCapacity #-}
modifyCapacity :: (HasCallStack, PrimMonad m, Num cap, Ord cap, VU.Unbox cap) => ACGV.GrowVec (PrimState m) (Int, Int, cap) -> (cap -> cap) -> Int -> m ()
modifyCapacity gv f i = do
  (VUM.MV_3 _ _ _ c) <- readMutVar $ ACGV.vecGV gv
  VUM.modify c f i

-- | \(O(1)\) Retrieves ith edge: @(from, to, capacity, flow)@.
getEdge :: (HasCallStack, PrimMonad m, Num cap, Ord cap, VU.Unbox cap) => MfGraph (PrimState m) cap -> Int -> m (Int, Int, cap, cap)
getEdge MfGraph {..} i = do
  len <- ACGV.length posG
  let !_ = runtimeAssert (0 <= i && i < len) "edge index out of bounds"
  (!from, !iEdge) <- ACGV.read posG i
  (!to, !iRevEdge, !cap) <- ACGV.read (gG VG.! from) iEdge
  revCap <- readCapacity gG to iRevEdge
  return (from, to, cap + revCap, revCap)

-- | \(O(1)\) Retrieves all the edges: @(from, to, capacity, flow)@.
edges :: (PrimMonad m, Num cap, Ord cap, VU.Unbox cap) => MfGraph (PrimState m) cap -> m (VU.Vector (Int, Int, cap, cap))
edges g@MfGraph {posG} = do
  len <- ACGV.length posG
  -- TODO: rewrite
  VU.generateM len (getEdge g)

-- TODO: changeEdge

-- | \(O(n^2m)\) (m: the number of edges)
flow :: (HasCallStack, Show cap, PrimMonad m, Bounded cap, Num cap, Ord cap, VU.Unbox cap) => MfGraph (PrimState m) cap -> Int -> Int -> m cap
flow gr s t = do
  flow' gr s t maxBound

flow' :: (HasCallStack, Show cap, PrimMonad m, Num cap, Ord cap, VU.Unbox cap) => MfGraph (PrimState m) cap -> Int -> Int -> cap -> m cap
flow' MfGraph {..} s t flowLimit = do
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
            (VUM.MV_3 _ vecTo _ vecCap) <- readMutVar $ ACGV.vecGV (gG VG.! v)
            len <- ACGV.length (gG VG.! v)
            neighbors <- VU.zip <$> VU.unsafeFreeze (VUM.take len vecTo) <*> VU.unsafeFreeze (VUM.take len vecCap)
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
            len <- ACGV.length (gG VG.! v)
            levelV <- VGM.read level v
            result <- flip fix 0 $ \loop res -> do
              i <- VGM.read iter v
              if i >= len
                then return res
                else do
                  VGM.write iter v $ i + 1
                  (!to, !iRevEdge, !_) <- ACGV.read (gG VG.! v) i
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

-- | \(O(n + m)\)
minCut :: (PrimMonad m, Num cap, Ord cap, VU.Unbox cap) => MfGraph (PrimState m) cap -> Int -> m (VU.Vector Bool)
minCut MfGraph {..} s = do
  visited <- VUM.replicate nG False
  que <- ACQ.new nG -- we could use a growable queue here
  ACQ.pushBack que s
  fix $ \loop -> do
    whenJustM (ACQ.popFront que) $ \p -> do
      VGM.write visited p True
      es <- ACGV.unsafeFreeze (gG VG.! p)
      VU.forM_ es $ \(!to, !_, !cap) -> do
        when (cap /= 0) $ do
          b <- VGM.exchange visited to True
          unless b $ do
            ACQ.pushBack que to
      loop
  VU.unsafeFreeze visited
