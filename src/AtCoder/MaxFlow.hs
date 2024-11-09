{-# LANGUAGE RecordWildCards #-}

-- | Maximum flow.
module AtCoder.MaxFlow (MfGraph, new, new', addEdge, addEdge_, getEdge, edges, changeEdge, flow, flow', minCut) where

import AtCoder.Internal.Assert qualified as ACIA
import AtCoder.Internal.GrowVec qualified as ACIGV
import AtCoder.Internal.Queue qualified as ACIQ
import Control.Monad (unless, when)
import Control.Monad.Extra (whenJustM)
import Control.Monad.Fix (fix)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Bit (Bit(..))
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
    gG :: !(V.Vector (ACIGV.GrowVec s (Int, Int, cap))),
    -- | Forward edge information: originalEdgeIndex -> (fromVertex, edgeIndex)
    posG :: !(ACIGV.GrowVec s (Int, Int))
  }

-- | \(O(n^2)\)
new :: (VU.Unbox cap, PrimMonad m) => Int -> m (MfGraph (PrimState m) cap)
new nG = do
  new' nG 0

-- | \(O(n + e)\)
new' :: (VU.Unbox cap, PrimMonad m) => Int -> Int -> m (MfGraph (PrimState m) cap)
new' nG nEdges = do
  gG <- V.replicateM nG (ACIGV.new 0)
  posG <- ACIGV.new nEdges
  return MfGraph {..}

-- | Amortized \(O(1)\)
addEdge :: (HasCallStack, PrimMonad m, Num cap, Ord cap, VU.Unbox cap) => MfGraph (PrimState m) cap -> Int -> Int -> cap -> m Int
addEdge MfGraph {..} from to cap = do
  let !_ = ACIA.checkCustom "AtCoder.MaxFlow.addEdge" "`from` vertex" from "the number of vertices" nG
  let !_ = ACIA.checkCustom "AtCoder.MaxFlow.addEdge" "`to` vertex" to "the number of vertices" nG
  let !_ = ACIA.runtimeAssert (0 <= cap) "AtCoder.MaxFlow.addEdge: given invalid `capacity` less than `0`" -- not `Show cap`
  m <- ACIGV.length posG
  iEdge <- ACIGV.length (gG VG.! from)
  ACIGV.pushBack posG (from, iEdge)
  iRevEdge <- do
    len <- ACIGV.length (gG VG.! to)
    return $ if from == to then len + 1 else len
  ACIGV.pushBack (gG VG.! from) (to, iRevEdge, cap)
  ACIGV.pushBack (gG VG.! to) (from, iEdge, 0)
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

-- | \(O(1)\) Retrieves ith edge: @(from, to, capacity, flow)@.
getEdge :: (HasCallStack, PrimMonad m, Num cap, Ord cap, VU.Unbox cap) => MfGraph (PrimState m) cap -> Int -> m (Int, Int, cap, cap)
getEdge MfGraph {..} i = do
  m <- ACIGV.length posG
  let !_ = ACIA.checkEdge "AtCoder.MaxFlow.getEdge" i m
  (!from, !iEdge) <- ACIGV.read posG i
  (!to, !iRevEdge, !cap) <- ACIGV.read (gG VG.! from) iEdge
  revCap <- readCapacity gG to iRevEdge
  return (from, to, cap + revCap, revCap)

-- | \(O(1)\) Retrieves all the edges: @(from, to, capacity, flow)@.
edges :: (PrimMonad m, Num cap, Ord cap, VU.Unbox cap) => MfGraph (PrimState m) cap -> m (VU.Vector (Int, Int, cap, cap))
edges g@MfGraph {posG} = do
  len <- ACIGV.length posG
  -- TODO: rewrite
  VU.generateM len (getEdge g)

-- TODO: changeEdge

-- | \(O(n^2m)\) (m: the number of edges)
flow :: (HasCallStack, Show cap, PrimMonad m, Bounded cap, Num cap, Ord cap, VU.Unbox cap) => MfGraph (PrimState m) cap -> Int -> Int -> m cap
flow gr s t = do
  flow' gr s t maxBound

flow' :: (HasCallStack, Show cap, PrimMonad m, Num cap, Ord cap, VU.Unbox cap) => MfGraph (PrimState m) cap -> Int -> Int -> cap -> m cap
flow' MfGraph {..} s t flowLimit = do
  let !_ = ACIA.checkCustom "AtCoder.MaxFlow.flow'" "`source` vertex" s "the number of vertices" nG
  let !_ = ACIA.checkCustom "AtCoder.MaxFlow.flow'" "`sink` vertex" t "the number of vertices" nG
  let !_ = ACIA.runtimeAssert (s /= t) $ "AtCoder.MaxFlow.flow': `source` and `sink` vertex have to be distinct: `" ++ show s ++ "`"

  level <- VUM.unsafeNew nG
  que <- ACIQ.new nG
  let bfs = do
        VGM.set level (-1 :: Int)
        VGM.write level s 0
        ACIQ.clear que
        ACIQ.pushBack que s
        fix $ \loop -> do
          whenJustM (ACIQ.popFront que) $ \v -> do
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
        | v == s = return up
        | otherwise = do
            len <- ACIGV.length (gG VG.! v)
            levelV <- VGM.read level v
            result <- flip fix 0 $ \loop res -> do
              i <- VGM.read iter v
              if i >= len
                then return res
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
minCut :: (PrimMonad m, Num cap, Ord cap, VU.Unbox cap) => MfGraph (PrimState m) cap -> Int -> m (VU.Vector Bit)
minCut MfGraph {..} s = do
  visited <- VUM.replicate nG $ Bit False
  que <- ACIQ.new nG -- we could use a growable queue here
  ACIQ.pushBack que s
  fix $ \loop -> do
    whenJustM (ACIQ.popFront que) $ \p -> do
      VGM.write visited p $ Bit True
      es <- ACIGV.unsafeFreeze (gG VG.! p)
      VU.forM_ es $ \(!to, !_, !cap) -> do
        when (cap /= 0) $ do
          Bit b <- VGM.exchange visited to $ Bit True
          unless b $ do
            ACIQ.pushBack que to
      loop
  VU.unsafeFreeze visited
