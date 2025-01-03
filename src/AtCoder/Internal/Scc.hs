{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_HADDOCK hide #-}

-- | Implementation of Strongly Connected Components calculation. Use `AtCoder.Scc` instead.
--
-- @since 1.0.0.0
module AtCoder.Internal.Scc
  ( -- * Internal SCC
    SccGraph (nScc),

    -- * Constructor
    new,

    -- * Adding edges
    addEdge,

    -- * SCC calculation
    sccIds,
    scc,
  )
where

import AtCoder.Internal.Csr qualified as ACICSR
import AtCoder.Internal.GrowVec qualified as ACIGV
import Control.Monad (unless, when)
import Control.Monad.Fix (fix)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Foldable (for_)
import Data.Maybe (fromJust)
import Data.Vector qualified as V
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM

-- | Graph for collecting strongly connected components.
--
-- @since 1.0.0.0
data SccGraph s = SccGraph
  { -- | The number of vertices.
    --
    -- @since 1.0.0.0
    nScc :: {-# UNPACK #-} !Int,
    edgesScc :: !(ACIGV.GrowVec s (Int, Int))
  }

-- | \(O(n)\) Creates a `SccGraph` of \(n\) vertices.
--
-- @since 1.0.0.0
{-# INLINE new #-}
new :: (PrimMonad m) => Int -> m (SccGraph (PrimState m))
new nScc = do
  edgesScc <- ACIGV.new 0
  pure SccGraph {..}

-- | \(O(1)\) amortized. Adds an edge to the graph.
--
-- @since 1.0.0.0
{-# INLINE addEdge #-}
addEdge :: (PrimMonad m) => SccGraph (PrimState m) -> Int -> Int -> m ()
addEdge SccGraph {edgesScc} from to = do
  ACIGV.pushBack edgesScc (from, to)

-- | \(O(n + m)\) Returns a pair of @(# of scc, scc id)@.
--
-- @since 1.0.0.0
{-# INLINE sccIds #-}
sccIds :: (PrimMonad m) => SccGraph (PrimState m) -> m (Int, VU.Vector Int)
sccIds SccGraph {..} = do
  -- see also the Wikipedia: https://en.wikipedia.org/wiki/Tarjan%27s_strongly_connected_components_algorithm#The_algorithm_in_pseudocode
  g <- ACICSR.build' nScc <$> ACIGV.unsafeFreeze edgesScc
  -- next SCC ID
  groupNum <- VUM.replicate 1 (0 :: Int)
  -- stack of vertices
  visited <- ACIGV.new nScc
  -- vertex -> low-link: the smallest index of any node on the stack known to be reachable from
  -- v through v's DFS subtree, including v itself.
  low <- VUM.replicate nScc (0 :: Int)
  -- vertex -> order of the visit (0, 1, ..)
  ord <- VUM.replicate nScc (-1 :: Int)
  -- vertex -> scc id
  ids <- VUM.replicate nScc (0 :: Int)

  let dfs v ord0 = do
        VGM.write low v ord0
        VGM.write ord v ord0
        ACIGV.pushBack visited v
        -- look around @v@, folding their low-link onto the low-link of @v@.
        ord' <-
          VU.foldM'
            ( \curOrd to -> do
                ordTo <- VGM.read ord to
                if ordTo == -1
                  then do
                    -- not visited yet.
                    nextOrd <- dfs to $ curOrd
                    lowTo <- VGM.read low to
                    VGM.modify low (min lowTo) v
                    pure nextOrd
                  else do
                    -- lookup back and update the low-link.
                    VGM.modify low (min ordTo) v
                    pure curOrd
            )
            (ord0 + 1)
            (g `ACICSR.adj` v)

        lowV <- VGM.read low v
        ordV <- VGM.read ord v
        when (lowV == ordV) $ do
          -- it's the root of a SCC, no more to look back
          sccId <- VGM.unsafeRead groupNum 0
          fix $ \loop -> do
            u <- fromJust <$> ACIGV.popBack visited
            VGM.write ord u nScc
            VGM.write ids u sccId
            unless (u == v) loop
          VGM.unsafeWrite groupNum 0 $ sccId + 1
        pure ord'

  VU.foldM'_
    ( \curOrd i -> do
        o <- VGM.read ord i
        if o == -1
          then dfs i curOrd
          else pure curOrd
    )
    (0 :: Int)
    (VU.generate nScc id)

  num <- VGM.unsafeRead groupNum 0
  -- The SCCs are reverse topologically sorted, e.g., [0, 1] <- [2] <- [3]
  -- Now reverse the SCC IDs so that they will be topologically sorted: [3] -> [2] -> [0, 1]
  for_ [0 .. nScc - 1] $ \i -> do
    VGM.modify ids ((num - 1) -) i

  ids' <- VU.unsafeFreeze ids
  pure (num, ids')

-- | \(O(n + m)\) Returns the strongly connected components.
--
-- @since 1.0.0.0
{-# INLINE scc #-}
scc :: (PrimMonad m) => SccGraph (PrimState m) -> m (V.Vector (VU.Vector Int))
scc g = do
  (!groupNum, !ids) <- sccIds g
  let counts = VU.create $ do
        vec <- VUM.replicate groupNum (0 :: Int)
        VU.forM_ ids $ \x -> do
          VGM.modify vec (+ 1) x
        pure vec
  groups <- V.mapM VUM.unsafeNew $ VU.convert counts
  is <- VUM.replicate groupNum (0 :: Int)
  VU.iforM_ ids $ \v sccId -> do
    i <- VGM.read is sccId
    VGM.write is sccId $ i + 1
    VGM.write (groups VG.! sccId) i v
  V.mapM VU.unsafeFreeze groups
