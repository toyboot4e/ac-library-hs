{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

-- | Re-export of the @Csr@ module and additional graph search functions.
--
-- @since 1.1.0.0
module AtCoder.Extra.Graph
  ( -- * Re-export of CSR

    -- | The `Csr.Csr` data type and all the functions such as `build` or `adj` are re-exported.
    -- See the @Csr@ module for details.
    module Csr,

    -- * CSR helpers
    swapDupe,
    swapDupe',
    scc,
    rev,

    -- * Generic graph functions
    topSort,
    connectedComponents,
    bipartiteVertexColors,
    blockCut,
    blockCutComponents,

    -- * Shortest path search

    -- | Most of the functions are opinionated as the followings:
    --
    -- - Indices are abstracted with `Ix0` (n-dimensional `Int`).
    -- - Functions that return a predecessor array are named as @tracking*@.

    -- ** BFS (breadth-first search)

    -- *** Constraints

    -- | - Edge weight \(w > 0\)
    bfs,
    trackingBfs,

    -- ** 01-BFS

    -- *** Constraints

    -- | - Edge weight \(w\) is either \(0\) or \(1\) of type `Int`.
    bfs01,
    trackingBfs01,

    -- ** Dijkstra's algorithm

    -- *** Constraints

    -- | - Edge weight \(w > 0\)
    dijkstra,
    trackingDijkstra,

    -- ** Bellman–ford algorithm

    -- | - Vertex type is restricted to one-dimensional `Int`.
    bellmanFord,
    trackingBellmanFord,

    -- ** Floyd–Warshall algorithm (all-pair shortest path)
    floydWarshall,
    trackingFloydWarshall,

    -- *** Incremental Floyd–Warshall algorithm
    newFloydWarshall,
    newTrackingFloydWarshall,
    updateEdgeFloydWarshall,
    updateEdgeTrackingFloydWarshall,

    -- ** Path reconstruction

    -- *** Single start point (root)

    -- | Functions for retrieving a path from a predecessor array where @-1@ represents none.
    constructPathFromRoot,
    constructPathToRoot,

    -- *** All-pair

    -- | Functions for retrieving a path from a predecessor matrix \(m\), which is accessed as
    -- @m VG.! (n * from + to)@, where @-1@ represents none.
    constructPathFromRootMat,
    constructPathToRootMat,
    constructPathFromRootMatM,
    constructPathToRootMatM,
  )
where

import AtCoder.Dsu qualified as Dsu
import AtCoder.Extra.IntSet qualified as IS
import AtCoder.Extra.Ix0 (Bounds0, Ix0 (..))
import AtCoder.Internal.Buffer qualified as B
import AtCoder.Internal.Csr as Csr
import AtCoder.Internal.MinHeap qualified as MH
import AtCoder.Internal.Queue qualified as Q
import AtCoder.Internal.Scc qualified as ACISCC
import Control.Monad (replicateM_, when)
import Control.Monad.Fix (fix)
import Control.Monad.Primitive (PrimMonad, PrimState, stToPrim)
import Control.Monad.ST (ST, runST)
import Data.Bit (Bit (..))
import Data.Foldable (for_)
import Data.Maybe (fromJust)
import Data.Vector qualified as V
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Stack (HasCallStack)

-- | \(O(n)\) Converts directed edges into non-directed edges; each edge \((u, v, w)\) is duplicated
-- to be \((u, v, w)\) and \((v, u, w)\). This is a convenient function for making an input to
-- `build`.
--
-- ==== __Example__
-- `swapDupe` duplicates each edge reversing the direction:
--
-- >>> import AtCoder.Extra.Graph qualified as Gr
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> Gr.swapDupe $ VU.fromList [(0, 1, ()), (1, 2, ())]
-- [(0,1,()),(1,0,()),(1,2,()),(2,1,())]
--
-- Create a non-directed graph:
--
-- >>> let gr = Gr.build 3 . Gr.swapDupe $ VU.fromList [(0, 1, ()), (1, 2, ())]
-- >>> gr `Gr.adj` 0
-- [1]
--
-- >>> gr `Gr.adj` 1
-- [0,2]
--
-- >>> gr `Gr.adj` 2
-- [1]
--
-- @since 1.1.0.0
{-# INLINEABLE swapDupe #-}
swapDupe :: (VU.Unbox w) => VU.Vector (Int, Int, w) -> VU.Vector (Int, Int, w)
swapDupe uvws = VU.create $ do
  vec <- VUM.unsafeNew (2 * VU.length uvws)
  VU.iforM_ uvws $ \i (!u, !v, !w) -> do
    VGM.unsafeWrite vec (2 * i + 0) (u, v, w)
    VGM.unsafeWrite vec (2 * i + 1) (v, u, w)
  pure vec

-- | \(O(n)\) Converts directed edges into non-directed edges; each edge \((u, v)\) is duplicated
-- to be \((u, v)\) and \((v, u)\). This is a convenient function for making an input to `build'`.
--
-- ==== __Example__
-- `swapDupe'` duplicates each edge reversing the direction:
--
-- >>> import AtCoder.Extra.Graph qualified as Gr
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> Gr.swapDupe' $ VU.fromList [(0, 1), (1, 2)]
-- [(0,1),(1,0),(1,2),(2,1)]
--
-- Create a non-directed graph:
--
-- >>> let gr = Gr.build' 3 . Gr.swapDupe' $ VU.fromList [(0, 1), (1, 2)]
-- >>> gr `Gr.adj` 0
-- [1]
--
-- >>> gr `Gr.adj` 1
-- [0,2]
--
-- >>> gr `Gr.adj` 2
-- [1]
--
-- @since 1.1.0.0
{-# INLINEABLE swapDupe' #-}
-- NOTE: concatMap does not fuse anyways, as the vector's code says
swapDupe' :: VU.Vector (Int, Int) -> VU.Vector (Int, Int)
swapDupe' uvs = VU.create $ do
  vec <- VUM.unsafeNew (2 * VU.length uvs)
  VU.iforM_ uvs $ \i (!u, !v) -> do
    VGM.unsafeWrite vec (2 * i + 0) (u, v)
    VGM.unsafeWrite vec (2 * i + 1) (v, u)
  pure vec

-- | \(O(n + m)\) Returns the strongly connected components of a `Csr`.
--
-- ==== __Example__
-- >>> import AtCoder.Extra.Graph qualified as Gr
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> -- 0 == 1 -> 2    3
-- >>> let gr = Gr.build' 4 $ VU.fromList [(0, 1), (1, 0), (1, 2)]
-- >>> Gr.scc gr
-- [[3],[0,1],[2]]
--
-- @since 1.1.0.0
{-# INLINE scc #-}
scc :: Csr w -> V.Vector (VU.Vector Int)
scc = ACISCC.sccCsr

-- | \(O(n + m)\) Returns a reverse graph, where original edges \((u, v, w)\) are transposed to be
-- \((v, u, w)\). Reverse graphs are useful for, for example, getting distance to a specific vertex
-- from every other vertex with `dijkstra`.
--
-- ==== __Example__
-- >>> import AtCoder.Extra.Graph qualified as Gr
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> -- 0 == 1 -> 2 -> 3
-- >>> let gr = Gr.build' 4 $ VU.fromList [(0, 1), (1, 0), (1, 2), (2, 3)]
-- >>> map (Gr.adj gr) [0 .. 3]
-- [[1],[0,2],[3],[]]
--
-- >>> -- 0 == 1 <- 2 <- 3
-- >>> let revGr = Gr.rev gr
-- >>> map (Gr.adj revGr) [0 .. 3]
-- [[1],[0],[1],[2]]
--
-- @since 1.2.3.0
{-# INLINEABLE rev #-}
rev :: (VU.Unbox w) => Csr w -> Csr w
rev Csr {..} = Csr.build nCsr revEdges
  where
    vws = VU.zip adjCsr wCsr
    revEdges = flip VU.concatMap (VU.generate nCsr id) $ \v1 ->
      let !o1 = startCsr VG.! v1
          !o2 = startCsr VG.! (v1 + 1)
          !vw2s = VU.slice o1 (o2 - o1) vws
       in VU.map (\(!v2, !w2) -> (v2, v1, w2)) vw2s

-- -------------------------------------------------------------------------------------------------
-- Generic graph search functions
-- -------------------------------------------------------------------------------------------------

-- | \(O(n \log n + m)\) Returns the lexicographically smallest topological ordering of the given
-- graph.
--
-- ==== Constraints
-- - The graph must be a DAG; no cycle can exist.
--
-- ==== __Example__
-- >>> import AtCoder.Extra.Graph qualified as Gr
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> let n = 5
-- >>> let gr = Gr.build' n $ VU.fromList [(1, 2), (4, 0), (0, 3)]
-- >>> Gr.topSort n (gr `Gr.adj`)
-- [1,2,4,0,3]
--
-- @since 1.1.0.0
{-# INLINEABLE topSort #-}
topSort :: Int -> (Int -> VU.Vector Int) -> VU.Vector Int
topSort n gr = runST $ do
  inDeg <- VUM.replicate n (0 :: Int)
  for_ [0 .. n - 1] $ \u -> do
    VU.forM_ (gr u) $ \v -> do
      VGM.modify inDeg (+ 1) v

  -- start from the vertices with zero in-degrees:
  que <- IS.new n
  inDeg' <- VU.unsafeFreeze inDeg
  VU.iforM_ inDeg' $ \v d -> do
    when (d == 0) $ do
      IS.insert que v

  buf <- B.new n
  fix $ \loop -> do
    IS.deleteMin que >>= \case
      Nothing -> pure ()
      Just u -> do
        B.pushBack buf u
        VU.forM_ (gr u) $ \v -> do
          nv <- subtract 1 <$> VGM.read inDeg v
          VGM.write inDeg v nv
          when (nv == 0) $ do
            IS.insert que v
        loop

  B.unsafeFreeze buf

-- | \(O(n)\) Returns connected components for a non-directed graph.
--
-- ==== Constraints
-- - The graph must be non-directed.
--
-- ==== __Example__
-- >>> import AtCoder.Extra.Graph qualified as Gr
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> let es = VU.fromList [(0, 1), (1, 2)]
-- >>> let gr = Gr.build' 4 $ Gr.swapDupe' es
-- >>> Gr.connectedComponents 4 (Gr.adj gr)
-- [[0,1,2],[3]]
--
-- >>> Gr.connectedComponents 0 (const VU.empty)
-- []
--
-- @since 1.2.4.0
{-# INLINEABLE connectedComponents #-}
connectedComponents :: Int -> (Int -> VU.Vector Int) -> V.Vector (VU.Vector Int)
connectedComponents n gr = runST $ do
  buf <- B.new @_ @Int n
  len <- B.new @_ @Int n
  vis <- VUM.replicate @_ @Bit n (Bit False)

  let dfs !acc u = do
        Bit b <- VGM.exchange vis u $ Bit True
        if b
          then pure acc
          else do
            B.pushBack buf u
            VU.foldM' dfs (acc + 1) (gr u)

  for_ [0 .. n - 1] $ \u -> do
    l :: Int <- dfs 0 u
    when (l > 0) $ do
      B.pushBack len l

  vs0 <- B.unsafeFreeze buf
  lens0 <- B.unsafeFreeze len

  pure
    . V.unfoldrExactN
      (VU.length lens0)
      ( \(!vs, !ls) ->
          let (!l, !lsR) = fromJust $ VU.uncons ls
              (!vsL, !vsR) = VU.splitAt l vs
           in (vsL, (vsR, lsR))
      )
    $ (vs0, lens0)

-- | \(O((n + m) \alpha)\) Returns a bipartite vertex coloring for a bipartite graph.
-- Returns `Nothing` for a non-bipartite graph.
--
-- ==== Constraints
-- - The graph must not be directed.
--
-- ==== __Example__
-- >>> import AtCoder.Extra.Graph qualified as Gr
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> let es = VU.fromList [(0, 1), (1, 2)]
-- >>> let gr = Gr.build' 4 es
-- >>> Gr.bipartiteVertexColors 4 (Gr.adj gr)
-- Just [0,1,0,0]
--
-- @since 1.2.4.0
{-# INLINEABLE bipartiteVertexColors #-}
bipartiteVertexColors :: Int -> (Int -> VU.Vector Int) -> Maybe (VU.Vector Bit)
bipartiteVertexColors n gr = runST $ do
  (!isBipartite, !color, !_) <- bipartiteVertexColorsImpl n gr
  if isBipartite
    then pure $ Just color
    else pure Nothing

{-# INLINEABLE bipartiteVertexColorsImpl #-}
bipartiteVertexColorsImpl :: Int -> (Int -> VU.Vector Int) -> ST s (Bool, VU.Vector Bit, Dsu.Dsu s)
bipartiteVertexColorsImpl n gr
  | n == 0 = do
      dsu <- Dsu.new 0
      pure (True, VU.empty, dsu)
  | otherwise = do
      -- 0 <= v < n: red, n <= v: green
      dsu <- Dsu.new (2 * n)
      for_ [0 .. n - 1] $ \u -> do
        VU.forM_ (gr u) $ \v -> do
          -- try both (red, green) and (green, red) colorings:
          Dsu.merge_ dsu (u + n) v
          Dsu.merge_ dsu u (v + n)

      color <- VUM.replicate (2 * n) $ Bit False

      -- for each leader vertices, paint their colors:
      for_ [0 .. n - 1] $ \v -> do
        l <- Dsu.leader dsu v
        when (l == v) $ do
          VGM.write color (v + n) $ Bit True

      -- paint other vertices:
      for_ [0 .. n - 1] $ \v -> do
        VGM.write color v =<< VGM.read color =<< Dsu.leader dsu v
        VGM.write color (v + n) =<< VGM.read color =<< Dsu.leader dsu (v + n)

      color' <- VU.unsafeFreeze $ VGM.take n color
      let isCompatible v
            | v >= n = pure True
            | otherwise = do
                c1 <- VGM.read color =<< Dsu.leader dsu v
                c2 <- VGM.read color =<< Dsu.leader dsu (v + n)
                if c1 == c2
                  then pure False
                  else isCompatible $ v + 1

      b <- isCompatible 0
      pure (b, color', dsu)

-- | \(O(n + m)\) Returns a [block cut tree](https://en.wikipedia.org/wiki/Biconnected_component)
-- where super vertices \((v \ge n)\) represent each biconnected component.
--
-- ==== __Example__
-- >>> import AtCoder.Extra.Graph qualified as Gr
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> -- 0---3---2
-- >>> -- +-1-+
-- >>> let n = 4
-- >>> let gr = Gr.build' n . Gr.swapDupe' $ VU.fromList [(0, 3), (0, 1), (1, 3), (3, 2)]
-- >>> let bct = blockCut n (gr `Gr.adj`)
-- >>> bct
-- Csr {nCsr = 6, mCsr = 5, startCsr = [0,0,0,0,0,2,5], adjCsr = [3,2,0,3,1], wCsr = [(),(),(),(),()]}
--
-- >>> V.generate (Gr.nCsr bct - n) ((bct `Gr.adj`) . (+ n))
-- [[3,2],[0,3,1]]
--
-- @since 1.1.1.0
{-# INLINEABLE blockCut #-}
blockCut :: Int -> (Int -> VU.Vector Int) -> Csr ()
blockCut n gr = runST $ do
  low <- VUM.replicate n (0 :: Int)
  ord <- VUM.replicate n (0 :: Int)
  st <- B.new @_ @Int n
  used <- VUM.replicate n $ Bit False
  edges <- B.new @_ @(Int, Int {- TODO: correct capacity? -}) (2 * n)
  -- represents the bidirected component's index. also works as super vertex indices.
  next <- VUM.replicate 1 n

  let dfs k0 v p = do
        B.pushBack st v
        VGM.write used v $ Bit True
        VGM.write low v k0
        VGM.write ord v k0

        snd
          <$> VU.foldM'
            ( \(!child, !k) to -> do
                if to == p
                  then pure (child, k)
                  else do
                    Bit b <- VGM.read used to
                    if not b
                      then do
                        let !child' = child + 1
                        s <- B.length st
                        k' <- dfs k to v
                        lowTo <- VGM.read low to
                        VGM.modify low (min lowTo) v
                        ordV <- VGM.read ord v
                        when ((p == -1 && child' > 1) || (p /= -1 && lowTo >= ordV)) $ do
                          nxt <- VGM.unsafeRead next 0
                          VGM.unsafeWrite next 0 (nxt + 1)
                          B.pushBack edges (nxt, v)
                          len <- B.length st
                          replicateM_ (len - s) $ do
                            back <- fromJust <$> B.popBack st
                            B.pushBack edges (nxt, back)
                        pure (child', k')
                      else do
                        ordTo <- VGM.read ord to
                        VGM.modify low (min ordTo) v
                        pure (child, k)
            )
            (0 :: Int, k0 + 1)
            (gr v)

  _ <-
    VGM.ifoldM'
      ( \k v (Bit b) -> do
          if b
            then do
              pure k
            else do
              k' <- dfs k v (-1)
              st' <- B.unsafeFreeze st
              nxt <- VGM.unsafeRead next 0
              VGM.unsafeWrite next 0 (nxt + 1)
              VU.forM_ st' $ \x -> do
                B.pushBack edges (nxt, x)
              B.clear st
              pure k'
      )
      (0 :: Int)
      used

  n' <- VGM.unsafeRead next 0
  Csr.build' n' <$> B.unsafeFreeze edges

-- | \(O(n + m)\) Returns [blocks (biconnected comopnents)](https://en.wikipedia.org/wiki/Biconnected_component)
-- of the graph.
--
-- ==== __Example__
-- >>> import AtCoder.Extra.Graph qualified as Gr
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> -- 0---3---2
-- >>> -- +-1-+
-- >>> let n = 4
-- >>> let gr = Gr.build' n . Gr.swapDupe' $ VU.fromList [(0, 3), (0, 1), (1, 3), (3, 2)]
-- >>> Gr.blockCutComponents n (gr `Gr.adj`)
-- [[3,2],[0,3,1]]
--
-- @since 1.1.1.0
{-# INLINEABLE blockCutComponents #-}
blockCutComponents :: Int -> (Int -> VU.Vector Int) -> V.Vector (VU.Vector Int)
blockCutComponents n gr =
  let bct = blockCut n gr
      d = nCsr bct - n
   in V.generate d ((bct `adj`) . (+ n))

-- -------------------------------------------------------------------------------------------------
-- Opinionated graph search functions
-- -------------------------------------------------------------------------------------------------

-- The implementations can be a bit simpler with `whenJustM`

-- | \(O(n + m)\) Opinionated breadth-first search that returns a distance array.
--
-- ==== __Example__
-- >>> import AtCoder.Extra.Graph qualified as Gr
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> let es = VU.fromList [(0, 1, 1 :: Int), (1, 2, 10)]
-- >>> let gr = Gr.build 4 es
-- >>> Gr.bfs 4 (Gr.adjW gr) (-1) (VU.singleton (0, 0))
-- [0,1,11,-1]
--
-- @since 1.2.4.0
{-# INLINE bfs #-}
bfs ::
  forall i w.
  (HasCallStack, Ix0 i, VU.Unbox i, VU.Unbox w, Num w, Eq w) =>
  -- | Zero-based vertex boundary.
  Bounds0 i ->
  -- | Graph function that takes a vertex and returns adjacent vertices with edge weights, where
  -- \(w > 0\).
  (i -> VU.Vector (i, w)) ->
  -- | Distance assignment for unreachable vertices.
  w ->
  -- | Weighted source vertices.
  VU.Vector (i, w) ->
  -- | Distance array in one-dimensional index.
  VU.Vector w
bfs !bnd0 !gr !undefW !sources =
  let (!dist, !_) = bfsImpl False bnd0 gr undefW sources
   in dist

-- | \(O(n + m)\) Opinionated breadth-first search that returns a distance array and a predecessor
-- array.
--
-- ==== __Example__
-- >>> import AtCoder.Extra.Graph qualified as Gr
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> let es = VU.fromList [(0, 1, 1 :: Int), (1, 2, 10)]
-- >>> let gr = Gr.build 4 es
-- >>> let (!dist, !prev) = Gr.trackingBfs 4 (Gr.adjW gr) (-1) (VU.singleton (0, 0))
-- >>> dist
-- [0,1,11,-1]
--
-- >>> Gr.constructPathFromRoot prev 2
-- [0,1,2]
--
-- @since 1.2.4.0
{-# INLINE trackingBfs #-}
trackingBfs ::
  forall i w.
  (HasCallStack, Ix0 i, VU.Unbox i, VU.Unbox w, Num w, Eq w) =>
  -- | Zero-based vertex boundary.
  Bounds0 i ->
  -- | Graph function that takes a vertex and returns adjacent vertices with edge weights, where
  -- \(w > 0\).
  (i -> VU.Vector (i, w)) ->
  -- | Distance assignment for unreachable vertices.
  w ->
  -- | Weighted source vertices.
  VU.Vector (i, w) ->
  -- | A tuple of (Distance vector in one-dimensional index, Predecessor array (@-1@ represents none)).
  (VU.Vector w, VU.Vector Int)
trackingBfs = bfsImpl True

{-# INLINEABLE bfsImpl #-}
bfsImpl ::
  forall i w.
  (HasCallStack, Ix0 i, VU.Unbox i, VU.Unbox w, Num w, Eq w) =>
  Bool ->
  Bounds0 i ->
  (i -> VU.Vector (i, w)) ->
  w ->
  VU.Vector (i, w) ->
  (VU.Vector w, VU.Vector Int)
bfsImpl !trackPrev !bnd0 !gr !undefW !sources
  | VU.null sources && trackPrev = (VU.replicate nVerts undefW, VU.replicate nVerts (-1))
  | VU.null sources = (VU.replicate nVerts undefW, VU.replicate 0 (-1))
  | otherwise = runST $ do
      dist <- VUM.replicate @_ @w nVerts undefW
      prev <-
        if trackPrev
          then VUM.replicate @_ @Int nVerts (-1)
          else VUM.replicate @_ @Int 0 (-1)

      -- NOTE: We only need capacity of `n`, as first appearance of vertex is always with the
      -- minimum distance.
      queue <- Q.new nVerts

      -- set source values
      VU.forM_ sources $ \(!src, !w0) -> do
        -- TODO: assert w1 <= w2
        let !i = index0 bnd0 src
        !lastD <- VGM.read dist i
        -- Note that duplicate inputs are pruned here:
        when (lastD == undefW) $ do
          VGM.write dist i w0
          Q.pushBack queue src

      -- run BFS
      fix $ \loop -> do
        Q.popFront queue >>= \case
          Nothing -> pure ()
          Just v1 -> do
            let !i1 = index0 bnd0 v1
            !d1 <- VGM.read dist i1
            VU.forM_ (gr v1) $ \(!v2, !dw) -> do
              let !i2 = index0 bnd0 v2
              !lastD <- VGM.read dist i2
              when (lastD == undefW) $ do
                VGM.write dist i2 $! d1 + dw
                when trackPrev $ do
                  VGM.write prev i2 i1
                Q.pushBack queue v2
            loop

      (,) <$> VU.unsafeFreeze dist <*> VU.unsafeFreeze prev
  where
    !nVerts = rangeSize0 bnd0

-- | \(O(n + m)\) Opinionated 01-BFS that returns a distance array.
--
-- ==== __Example__
-- >>> import AtCoder.Extra.Graph qualified as Gr
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> let es = VU.fromList [(0, 1, 10 :: Int), (0, 2, 0), (2, 1, 1)]
-- >>> let gr = Gr.build 4 es
-- >>> let capacity = 10
-- >>> Gr.bfs01 4 (Gr.adjW gr) capacity (VU.singleton (0, 0))
-- [0,1,0,-1]
--
-- @since 1.2.4.0
{-# INLINE bfs01 #-}
bfs01 ::
  forall i.
  (HasCallStack, Ix0 i, VU.Unbox i) =>
  -- | Zero-based index boundary.
  Bounds0 i ->
  -- | Graph function that takes the vertexand returns adjacent vertices with edge weights, where
  -- \(w > 0\).
  (i -> VU.Vector (i, Int)) ->
  -- | Capacity of deque, often the number of edges \(m\).
  Int ->
  -- | Weighted source vertices.
  VU.Vector (i, Int) ->
  -- | Distance array in one-dimensional index. Unreachable vertices are assigned distance of @-1@.
  VU.Vector Int
bfs01 !bnd0 !gr !capacity !sources =
  let (!dist, !_) = bfs01Impl False bnd0 gr capacity sources
   in dist

-- | \(O(n + m)\) Opinionated 01-BFS that returns a distance array and a predecessor array.
--
-- ==== __Example__
-- >>> import AtCoder.Extra.Graph qualified as Gr
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> let es = VU.fromList [(0, 1, 10 :: Int), (0, 2, 0), (2, 1, 1)]
-- >>> let gr = Gr.build 4 es
-- >>> let capacity = 10
-- >>> let (!dist, !prev) = Gr.trackingBfs01 4 (Gr.adjW gr) capacity (VU.singleton (0, 0))
-- >>> dist
-- [0,1,0,-1]
--
-- >>> Gr.constructPathFromRoot prev 1
-- [0,2,1]
--
-- @since 1.2.4.0
{-# INLINE trackingBfs01 #-}
trackingBfs01 ::
  forall i.
  (HasCallStack, Ix0 i, VU.Unbox i) =>
  -- | Zero-based index boundary.
  Bounds0 i ->
  -- | Graph function that takes the vertex and returns adjacent vertices with edge weights, where
  -- \(w > 0\).
  (i -> VU.Vector (i, Int)) ->
  -- | Capacity of deque, often the number of edges \(m\).
  Int ->
  -- | Weighted source vertices.
  VU.Vector (i, Int) ->
  -- | A tuple of (distance array in one-dimensional index, predecessor array). Unreachable vertices
  -- are assigned distance of @-1@.
  (VU.Vector Int, VU.Vector Int)
trackingBfs01 = bfs01Impl True

{-# INLINEABLE bfs01Impl #-}
bfs01Impl ::
  forall i.
  (HasCallStack, Ix0 i, VU.Unbox i) =>
  Bool ->
  Bounds0 i ->
  (i -> VU.Vector (i, Int)) ->
  Int ->
  VU.Vector (i, Int) ->
  (VU.Vector Int, VU.Vector Int)
bfs01Impl !trackPrev !bnd0 !gr !capacity !sources
  | VU.null sources && trackPrev = (VU.replicate nVerts (-1), VU.replicate nVerts (-1))
  | VU.null sources = (VU.replicate nVerts (-1), VU.replicate 0 (-1))
  | otherwise = runST $ do
      dist <- VUM.replicate @_ @Int nVerts undef
      prev <-
        if trackPrev
          then VUM.replicate @_ @Int nVerts (-1)
          else VUM.replicate @_ @Int 0 (-1)
      -- NOTE: Just like Dijkstra, we need capacity of `m`, as the first appearance of a vertex is not
      -- always with minimum distance.
      deque <- Q.newDeque @_ @(i, Int) capacity

      -- set source values
      VU.forM_ sources $ \(!src, !w0) -> do
        -- TODO: assert x1 <= w2
        let !i = index0 bnd0 src
        !lastD <- VGM.read dist i
        -- Note that duplicate inputs are pruned here:
        when (lastD == undef) $ do
          VGM.write dist i w0
          Q.pushBack deque (src, w0)

      let step !vExt0 !w0 = do
            let !i0 = index0 bnd0 vExt0
            !wReserved0 <- VGM.read dist i0
            when (w0 == wReserved0) $ do
              VU.forM_ (gr vExt0) $ \(!vExt, !dw) -> do
                let !w = w0 + dw
                let !i = index0 bnd0 vExt
                !wReserved <- VGM.read dist i
                -- NOTE: Do pruning just like Dijkstra:
                when (wReserved == undef || w < wReserved) $ do
                  VGM.write dist i w
                  when trackPrev $ do
                    VGM.write prev i i0
                  if dw == 0
                    then Q.pushFront deque (vExt, w)
                    else Q.pushBack deque (vExt, w)

      fix $ \popLoop -> do
        Q.popFront deque >>= \case
          Nothing -> pure ()
          Just (!vExt0, !w0) -> do
            step vExt0 w0
            popLoop

      (,) <$> VU.unsafeFreeze dist <*> VU.unsafeFreeze prev
  where
    !undef = -1 :: Int
    !nVerts = rangeSize0 bnd0

-- | \(O((n + m) \log n)\) Dijkstra's algorithm that returns a distance array.
--
-- ==== __Example__
-- >>> import AtCoder.Extra.Graph qualified as Gr
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> let es = VU.fromList [(0, 1, 10 :: Int), (1, 2, 20), (2, 3, 1), (1, 3, 40), (4, 3, 0)]
-- >>> let gr = Gr.build 5 es
-- >>> let capacity = 10
-- >>> Gr.dijkstra 5 (Gr.adjW gr) capacity (-1) (VU.singleton (0, 0))
-- [0,10,30,31,-1]
--
-- @since 1.2.4.0
{-# INLINE dijkstra #-}
dijkstra ::
  forall i w.
  (HasCallStack, Ix0 i, Ord i, VU.Unbox i, Num w, Ord w, VU.Unbox w) =>
  -- | Zero-based vertex boundary.
  Bounds0 i ->
  -- | Graph function that takes a vertex and returns adjacent vertices with edge weights, where
  -- \(w \ge 0\).
  (i -> VU.Vector (i, w)) ->
  -- | Capacity of the heap, often the number of edges \(m\).
  Int ->
  -- | Distance assignment for unreachable vertices.
  w ->
  -- | Source vertices with initial weights.
  VU.Vector (i, w) ->
  -- | Distance array in one-dimensional index.
  VU.Vector w
dijkstra !bnd0 !gr !capacity !undefW !sources =
  let (!dist, !_) = dijkstraImpl False bnd0 gr capacity undefW sources
   in dist

-- | \(O((n + m) \log n)\) Dijkstra's algorithm that returns a distance array and a predecessor
-- array.
--
-- ==== __Example__
-- >>> import AtCoder.Extra.Graph qualified as Gr
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> let es = VU.fromList [(0, 1, 10 :: Int), (1, 2, 20), (2, 3, 1), (1, 3, 40), (4, 3, 0)]
-- >>> let gr = Gr.build 5 es
-- >>> let capacity = 10
-- >>> let (!dist, !prev) = Gr.trackingDijkstra 5 (Gr.adjW gr) capacity (-1) (VU.singleton (0, 0))
-- >>> dist
-- [0,10,30,31,-1]
--
-- >>> Gr.constructPathFromRoot prev 3
-- [0,1,2,3]
--
-- @since 1.2.4.0
{-# INLINE trackingDijkstra #-}
trackingDijkstra ::
  forall i w.
  (HasCallStack, Ix0 i, Ord i, VU.Unbox i, Num w, Ord w, VU.Unbox w) =>
  -- | Zero-based vertex boundary.
  Bounds0 i ->
  -- | Graph function that takes a vertex and returns adjacent vertices with edge weights, where
  -- \(w \ge 0\).
  (i -> VU.Vector (i, w)) ->
  -- | Capacity of the heap, often the number of edges \(m\).
  Int ->
  -- | Distance assignment for unreachable vertices.
  w ->
  -- | Source vertices with weights.
  VU.Vector (i, w) ->
  -- | A tuple of (distance array in one-dimensional index, predecessor array).
  (VU.Vector w, VU.Vector Int)
trackingDijkstra = dijkstraImpl True

{-# INLINEABLE dijkstraImpl #-}
dijkstraImpl ::
  forall i w.
  (HasCallStack, Ix0 i, Ord i, VU.Unbox i, Num w, Ord w, VU.Unbox w) =>
  Bool ->
  Bounds0 i ->
  (i -> VU.Vector (i, w)) ->
  Int ->
  w ->
  VU.Vector (i, w) ->
  (VU.Vector w, VU.Vector Int)
dijkstraImpl !trackPrev !bnd0 !gr !capacity !undefW !sources
  | VU.null sources && trackPrev = (VU.replicate nVerts undefW, VU.replicate nVerts (-1))
  | VU.null sources = (VU.replicate nVerts undefW, VU.replicate 0 (-1))
  | otherwise = runST $ do
      !dist <- VUM.replicate @_ @w nVerts undefW
      -- REMARK: (w, i) for sort by width
      !heap <- MH.new @_ @(w, i) capacity
      !prev <-
        if trackPrev
          then VUM.replicate @_ @Int nVerts (-1)
          else VUM.replicate @_ @Int 0 (-1)

      VU.forM_ sources $ \(!v, !w) -> do
        let !i = index0 bnd0 v
        VGM.write dist i w
        MH.push heap (w, v)

      fix $ \loop -> do
        MH.pop heap >>= \case
          Nothing -> pure ()
          Just (!w1, !v1) -> do
            let !i1 = index0 bnd0 v1
            !wReserved <- VGM.read dist i1
            when (wReserved == w1) $ do
              VU.forM_ (gr v1) $ \(!v2, !dw2) -> do
                let !i2 = index0 bnd0 v2
                !w2 <- VGM.read dist i2
                let !w2' = w1 + dw2
                when (w2 == undefW || w2' < w2) $ do
                  VGM.write dist i2 w2'
                  when trackPrev $ do
                    VGM.write prev i2 i1
                  MH.push heap (w2', v2)
            loop

      (,) <$> VU.unsafeFreeze dist <*> VU.unsafeFreeze prev
  where
    !nVerts = rangeSize0 bnd0

-- -- | Option for `bellmanFord`.
-- data BellmanFordPolicy = QuitOnNegaitveLoop | ContinueOnNegaitveLoop

-- | \(O(nm)\) Bellman–ford algorithm that returns a distance array, or `Nothing` on negative loop
-- detection. Vertices are one-dimensional.
--
-- ==== __Example__
-- >>> import AtCoder.Extra.Graph qualified as Gr
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> let gr = Gr.build @Int 5 $ VU.fromList [(0, 1, 10), (1, 2, -20), (2, 3, 1), (1, 3, 40), (4, 3, 0)]
-- >>> let undefW = maxBound `div` 2
-- >>> Gr.bellmanFord 5 (Gr.adjW gr) undefW (VU.singleton (0, 0))
-- Just [0,10,-10,-9,4611686018427387903]
--
-- It returns `Nothing` on negative loop detection:
--
-- >>> let gr = Gr.build @Int 2 $ VU.fromList [(0, 1, -1), (1, 0, -1)]
-- >>> Gr.bellmanFord 5 (Gr.adjW gr) undefW (VU.singleton (0, 0))
-- Nothing
--
-- @since 1.2.4.0
{-# INLINE bellmanFord #-}
bellmanFord ::
  forall w.
  (HasCallStack, Num w, Ord w, VU.Unbox w) =>
  -- | The number of vertices.
  Int ->
  -- | Graph function. Edges weights can be negative.
  (Int -> VU.Vector (Int, w)) ->
  -- | Distance assignment for unreachable vertices.
  w ->
  -- | Source vertex with initial distances.
  VU.Vector (Int, w) ->
  -- | Distance array in one-dimensional index.
  Maybe (VU.Vector w)
bellmanFord {- !policy -} !nVerts !gr !undefW source = do
  (!dist, !_) <- bellmanFordImpl False nVerts gr undefW source
  pure dist

-- | \(O(nm)\) Bellman–ford algorithm that returns a distance array and a predecessor array, or
-- `Nothing` on negative loop detection. Vertices are one-dimensional.
--
-- ==== __Example__
-- >>> import AtCoder.Extra.Graph qualified as Gr
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> let gr = Gr.build @Int 5 $ VU.fromList [(0, 1, 10), (1, 2, -20), (2, 3, 1), (1, 3, 40), (4, 3, 0)]
-- >>> let undefW = maxBound `div` 2
-- >>> let Just (!dist, !prev) = Gr.trackingBellmanFord 5 (Gr.adjW gr) undefW (VU.singleton (0, 0))
-- >>> dist
-- [0,10,-10,-9,4611686018427387903]
--
-- >>> Gr.constructPathFromRoot prev 3
-- [0,1,2,3]
--
-- It returns `Nothing` on negative loop detection:
--
-- >>> let gr = Gr.build @Int 2 $ VU.fromList [(0, 1, -1), (1, 0, -1)]
-- >>> Gr.trackingBellmanFord 5 (Gr.adjW gr) undefW (VU.singleton (0, 0))
-- Nothing
--
-- @since 1.2.4.0
{-# INLINE trackingBellmanFord #-}
trackingBellmanFord ::
  forall w.
  (HasCallStack, Num w, Ord w, VU.Unbox w) =>
  -- | The number of vertices.
  Int ->
  -- | Graph function. The weight can be negative.
  (Int -> VU.Vector (Int, w)) ->
  -- | Distance assignment for unreachable vertices.
  w ->
  -- | Source vertex with initial distances.
  VU.Vector (Int, w) ->
  -- | A tuple of (distance array, predecessor array).
  Maybe (VU.Vector w, VU.Vector Int)
trackingBellmanFord {- !policy -} = bellmanFordImpl True

{-# INLINEABLE bellmanFordImpl #-}
bellmanFordImpl ::
  forall w.
  (HasCallStack, Num w, Ord w, VU.Unbox w) =>
  Bool ->
  Int ->
  (Int -> VU.Vector (Int, w)) ->
  w ->
  VU.Vector (Int, w) ->
  Maybe (VU.Vector w, VU.Vector Int)
bellmanFordImpl {- !policy -} !trackPrev !nVerts !gr !undefW !sources = runST $ do
  !dist <- VUM.replicate @_ @w nVerts undefW
  !prev <-
    if trackPrev
      then VUM.replicate @_ @Int nVerts (-1)
      else VUM.replicate @_ @Int 0 (-1)

  VU.forM_ sources $ \(!v, !w) -> do
    !lastD <- VGM.read dist v
    -- Note that duplicate inputs are pruned here:
    when (lastD == undefW) $ do
      VGM.write dist v w
  updated <- VUM.replicate 1 False

  -- look around adjaenct vertices
  let update v1 = do
        d1 <- VGM.read dist v1
        when (d1 /= undefW) $ do
          VU.forM_ (gr v1) $ \(!v2, !dw) -> do
            d2 <- VGM.read dist v2
            let !d2' = d1 + dw
            when (d2 == undefW || d2' < d2) $ do
              VGM.write dist v2 d2'
              when trackPrev $ do
                VGM.write prev v2 v1
              -- NOTE: we should actually instantly stop if nLoop == nVerts + 1, but
              -- here we're preferring simple code. Be warned that we're not correctly handling
              -- the distance array on negative loop.
              VGM.write updated 0 True

  let runLoop nLoop
        | nLoop >= nVerts + 1 = do
            -- We detected update in the (n + 1)-th loop, so we found negative loop
            pure Nothing
        | otherwise = do
            for_ [0 .. nVerts - 1] update
            b <- VGM.exchange updated 0 False
            if b
              then runLoop (nLoop + 1)
              else Just <$> ((,) <$> VU.unsafeFreeze dist <*> VU.unsafeFreeze prev)

  runLoop 0

-- | \(O(n^3)\) Floyd–Warshall algorithm that returns a distance matrix \(m\), which should be
-- accessed as @m VU.! (`index0` (n, n) (from, to))@. Negative loop can be detected by testing if
-- there's any vertex \(v\) where @m VU.! (`index0` (n, n) (v, v))@.
--
-- ==== __Example__
-- >>> import AtCoder.Extra.Graph qualified as Gr
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> let es = VU.fromList [(0, 1, 10 :: Int), (1, 2, -20), (2, 3, 1), (1, 3, 40), (4, 3, 0)]
-- >>> let undefW = maxBound `div` 2
-- >>> let dist = Gr.floydWarshall 5 es undefW
-- >>> dist VG.! (5 * 0 + 3) -- from `0` to `3`
-- -9
--
-- >>> dist VG.! (5 * 1 + 3) -- from `0` to `3`
-- -19
--
-- Negative loop can be detected by testing if there's any vertex \(v\) where
-- @m VU.! (`index0` (n, n) (v, v))@:
--
-- >>> any (\v -> dist VG.! (5 * v + v) < 0) [0 .. 5 - 1]
-- False
--
-- >>> let es = VU.fromList [(0, 1, -1 :: Int), (1, 0, -1)]
-- >>> let dist = Gr.floydWarshall 3 es undefW
-- >>> any (\v -> dist VG.! (3 * v + v) < 0) [0 .. 3 - 1]
-- True
--
-- @since 1.2.4.0
{-# INLINE floydWarshall #-}
floydWarshall ::
  forall w.
  (HasCallStack, Num w, Ord w, VU.Unbox w) =>
  -- | The number of vertices.
  Int ->
  -- | Weighted edges.
  VU.Vector (Int, Int, w) ->
  -- | Distance assignment \(d_0 \gt 0\) for unreachable vertices. It should be @maxBound `div` 2@
  -- for `Int`.
  w ->
  -- | Distance array in one-dimensional index.
  VU.Vector w
floydWarshall !nVerts !edges !undefW = VU.create $ do
  (!dist, !_) <- newFloydWarshallST False nVerts edges undefW
  pure dist

-- | \(O(n^3)\) Floyd–Warshall algorithm that returns a distance matrix \(m\) and predecessor
-- matrix \(p\). The distance matrix should be accessed as @m VU.! (`index0` (n, n) (from, to))@,
-- and the predecessor matrix should be accessed as @m VU.! (`index0` (n, n) (root, v))@. There's a
-- negative loop if there's any vertex \(v\) where @m VU.! (`index0` (n, n) (v, v))@.
--
-- ==== __Example__
-- >>> import AtCoder.Extra.Graph qualified as Gr
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> let es = VU.fromList [(0, 1, 10 :: Int), (1, 2, -20), (2, 3, 1), (1, 3, 40), (4, 3, 0)]
-- >>> let undefW = maxBound `div` 2
-- >>> let (!dist, !prev) = Gr.trackingFloydWarshall 5 es undefW
-- >>> dist VG.! (5 * 0 + 3) -- from `0` to `3`
-- -9
--
-- >>> Gr.constructPathFromRootMat prev 0 3 -- from `0` to `3`
-- [0,1,2,3]
--
-- >>> dist VG.! (5 * 1 + 3) -- from `0` to `3`
-- -19
--
-- >>> Gr.constructPathFromRootMat prev 1 3 -- from `1` to `3`
-- [1,2,3]
--
-- Negative loop can be detected by testing if there's any vertex \(v\) where
-- @m VU.! (`index0` (n, n) (v, v))@:
--
-- >>> any (\v -> dist VG.! (5 * v + v) < 0) [0 .. 5 - 1]
-- False
--
-- >>> let es = VU.fromList [(0, 1, -1 :: Int), (1, 0, -1)]
-- >>> let (!dist, !_) = Gr.trackingFloydWarshall 3 es undefW
-- >>> any (\v -> dist VG.! (3 * v + v) < 0) [0 .. 3 - 1]
-- True
--
-- @since 1.2.4.0
{-# INLINE trackingFloydWarshall #-}
trackingFloydWarshall ::
  forall w.
  (HasCallStack, Num w, Ord w, VU.Unbox w) =>
  -- | The number of vertices.
  Int ->
  -- | Weighted edges.
  VU.Vector (Int, Int, w) ->
  -- | Distance assignment \(d_0 \gt 0\) for unreachable vertices. It should be @maxBound `div` 2@
  -- for `Int`.
  w ->
  -- | Distance array in one-dimensional index.
  (VU.Vector w, VU.Vector Int)
trackingFloydWarshall !nVerts !edges !undefW = runST $ do
  (!dist, !prev) <- newFloydWarshallST True nVerts edges undefW
  (,) <$> VU.unsafeFreeze dist <*> VU.unsafeFreeze prev

-- | \(O(n^3)\) Floyd–Warshall algorithm that returns a distance matrix \(m\), which should be
-- accessed as @m VU.! (n * from + to)@. There's a negative cycle if any @m VU.! (n * i + i)@ is
-- negative.
--
-- ==== __Example__
-- >>> import AtCoder.Extra.Graph qualified as Gr
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> let es = VU.fromList [(0, 1, 1 :: Int), (1, 2, 1), (2, 3, 1), (1, 3, 4)]
-- >>> let undefW = -1
-- >>> dist <- Gr.newFloydWarshall 4 es undefW
-- >>> VGM.read dist (4 * 0 + 3)
-- 3
--
-- >>> updateEdgeFloydWarshall dist 4 undefW 1 3 (-2)
-- >>> VGM.read dist (4 * 0 + 3)
-- -1
--
-- @since 1.2.4.0
{-# INLINE newFloydWarshall #-}
newFloydWarshall ::
  forall m w.
  (HasCallStack, PrimMonad m, Num w, Ord w, VU.Unbox w) =>
  -- | The number of vertices.
  Int ->
  -- | Weighted edges.
  VU.Vector (Int, Int, w) ->
  -- | Distance assignment for unreachable vertices.
  w ->
  -- | Distance array in one-dimensional index.
  m (VUM.MVector (PrimState m) w)
newFloydWarshall !nVerts !edges !undefW = stToPrim $ do
  (!dist, !_) <- newFloydWarshallST False nVerts edges undefW
  pure dist

-- | \(O(n^3)\) Floyd–Warshall algorithm that returns a distance matrix \(m\) and predecessor
-- matrix. There's a negative cycle if any @m VU.! (n * i + i)@ is negative.
--
-- ==== __Example__
-- >>> import AtCoder.Extra.Graph qualified as Gr
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> let es = VU.fromList [(0, 1, 1 :: Int), (1, 2, 1), (2, 3, 1), (1, 3, 4)]
-- >>> let undefW = -1
-- >>> (!dist, !prev) <- Gr.newTrackingFloydWarshall 4 es undefW
-- >>> VGM.read dist (4 * 0 + 3)
-- 3
--
-- >>> constructPathFromRootMatM prev 0 3
-- [0,1,2,3]
--
-- >>> updateEdgeTrackingFloydWarshall dist prev 4 undefW 1 3 (-2)
-- >>> VGM.read dist (4 * 0 + 3)
-- -1
--
-- >>> constructPathFromRootMatM prev 0 3
-- [0,1,3]
--
-- @since 1.2.4.0
{-# INLINE newTrackingFloydWarshall #-}
newTrackingFloydWarshall ::
  forall m w.
  (HasCallStack, PrimMonad m, Num w, Ord w, VU.Unbox w) =>
  -- | The number of vertices.
  Int ->
  -- | Weighted edges.
  VU.Vector (Int, Int, w) ->
  -- | Distance assignment for unreachable vertices.
  w ->
  -- | Distance array in one-dimensional index.
  m (VUM.MVector (PrimState m) w, VUM.MVector (PrimState m) Int)
newTrackingFloydWarshall !nVerts !edges !undefW = stToPrim $ do
  newFloydWarshallST True nVerts edges undefW

{-# INLINEABLE newFloydWarshallST #-}
newFloydWarshallST ::
  forall s w.
  (HasCallStack, Num w, Ord w, VU.Unbox w) =>
  Bool ->
  Int ->
  VU.Vector (Int, Int, w) ->
  w ->
  ST s (VUM.MVector s w, VUM.MVector s Int)
newFloydWarshallST !trackPrev !nVerts !edges !undefW = do
  !dist <- VUM.replicate @_ @w (nVerts * nVerts) undefW
  !prev <-
    if trackPrev
      then VUM.replicate @_ @Int (nVerts * nVerts) (-1)
      else VUM.replicate @_ @Int 0 (-1)

  -- diagonals (self to self)
  for_ [0 .. nVerts - 1] $ \v -> do
    VGM.write dist (idx v v) 0

  -- initial walks
  VU.forM_ edges $ \(!v1, !v2, !dw) -> do
    let !i = idx v1 v2
    wOld <- VGM.read dist i
    -- REMARK: We're handling multiple edges here:
    when (wOld == undefW || dw < wOld) $ do
      VGM.write dist i dw
      when trackPrev $ do
        VGM.write prev i v1

  -- N times update
  for_ [0 .. nVerts - 1] $ \via -> do
    -- update
    for_ [0 .. nVerts - 1] $ \from -> do
      for_ [0 .. nVerts - 1] $ \to -> do
        let !iFromTo = idx from to
        !w1 <- VGM.read dist iFromTo
        !w2 <- do
          !d1 <- VGM.read dist $! idx from via
          !d2 <- VGM.read dist $! idx via to
          pure $! if d1 == undefW || d2 == undefW then undefW else d1 + d2
        when (w2 /= undefW && (w1 == undefW || w2 < w1)) $ do
          VGM.write dist iFromTo w2
          when trackPrev $ do
            VGM.write prev iFromTo =<< VGM.read prev (idx via to)

  pure (dist, prev)
  where
    idx !from !to = nVerts * from + to

-- | \(O(n^2)\) Updates distance matrix of Floyd–Warshall on edge weight decreasement or new edge
-- addition.
--
-- @since 1.2.4.0
{-# INLINE updateEdgeFloydWarshall #-}
updateEdgeFloydWarshall ::
  forall m w.
  (HasCallStack, PrimMonad m, Num w, Ord w, VU.Unbox w) =>
  -- | Distance matrix.
  VUM.MVector (PrimState m) w ->
  -- | The number of vertices.
  Int ->
  -- | Distance assignment \(d_0 \gt 0\) for unreachable vertices. It should be @maxBound `div` 2@
  -- for `Int`.
  w ->
  -- | Edge information: @from@ vertex.
  Int ->
  -- | Edge information: @to@ vertex.
  Int ->
  -- | Edge information: @weight@ vertex.
  w ->
  -- | Distance array in one-dimensional index.
  m ()
updateEdgeFloydWarshall mat nVerts undefW a b w = do
  prev <- VUM.replicate @_ @Int 0 (-1 :: Int)
  stToPrim $ updateEdgeFloydWarshallST False mat prev nVerts undefW a b w

-- | \(O(n^2)\) Updates distance matrix of Floyd–Warshall on edge weight decreasement or new edge
-- addition.
--
-- @since 1.2.4.0
{-# INLINE updateEdgeTrackingFloydWarshall #-}
updateEdgeTrackingFloydWarshall ::
  forall m w.
  (HasCallStack, PrimMonad m, Num w, Ord w, VU.Unbox w) =>
  -- | Distance matrix.
  VUM.MVector (PrimState m) w ->
  -- | Predecessor matrix.
  VUM.MVector (PrimState m) Int ->
  -- | The number of vertices.
  Int ->
  -- | Distance assignment \(d_0 \gt 0\) for unreachable vertices. It should be @maxBound `div` 2@
  -- for `Int`.
  w ->
  -- | Edge information: @from@ vertex.
  Int ->
  -- | Edge information: @to@ vertex.
  Int ->
  -- | Edge information: @weight@ vertex.
  w ->
  -- | Distance array in one-dimensional index.
  m ()
updateEdgeTrackingFloydWarshall mat prev nVerts undefW a b w = do
  stToPrim $ updateEdgeFloydWarshallST True mat prev nVerts undefW a b w

-- O(2) update floyd warshall on edge weight decreasement or edge addition
-- https://www.slideshare.net/chokudai/arc035 - C
{-# INLINEABLE updateEdgeFloydWarshallST #-}
updateEdgeFloydWarshallST ::
  forall s w.
  (HasCallStack, Num w, Ord w, VU.Unbox w) =>
  Bool ->
  VUM.MVector s w ->
  VUM.MVector s Int ->
  Int ->
  w ->
  Int ->
  Int ->
  w ->
  ST s ()
updateEdgeFloydWarshallST trackPrev mat prev nVerts undefW a b dw = do
  wOld0 <- VGM.read mat $! idx a b
  when (wOld0 == undefW || dw < wOld0) $ do
    VGM.write mat (idx a b) dw
    when trackPrev $ do
      VGM.write prev (idx a b) a
    for_ [0 .. nVerts - 1] $ \from -> do
      for_ [0 .. nVerts - 1] $ \to -> do
        wOld <- VGM.read mat $! idx from to

        w' <- do
          ia <- VGM.read mat $! idx from a
          bj <- VGM.read mat $! idx b to
          let w1
                | ia == undefW || bj == undefW = undefW
                | otherwise = ia + dw + bj

          ib <- VGM.read mat $! idx from b
          aj <- VGM.read mat $! idx a to
          let w2
                | ib == undefW || aj == undefW = undefW
                | otherwise = ib + dw + aj

          pure $!
            if
              | w1 == undefW -> w2
              | w2 == undefW -> w1
              | otherwise -> min w1 w2

        when (wOld /= undefW && w' < wOld) $ do
          VGM.write mat (idx from to) w'
          when trackPrev $ do
            VGM.write prev (idx from to) =<< VGM.read prev (idx b to)
            VGM.write prev (idx from b) a
  where
    idx !from !to = nVerts * from + to

-- | \(O(n)\) Given a predecessor array, retrieves a path from the root to a vertex.
--
-- ==== Constraints
-- - The path must not make a cycle, otherwise this function loops forever.
-- - There must be a path from the root to the @end@ vertex, otherwise the returned path is not
-- connected to the root.
--
-- @since 1.2.4.0
{-# INLINE constructPathFromRoot #-}
constructPathFromRoot :: (HasCallStack) => VU.Vector Int -> Int -> VU.Vector Int
constructPathFromRoot parents = VU.reverse . constructPathToRoot parents

-- | \(O(n)\) Given a predecessor array, retrieves a path from a vertex to the root.
--
-- ==== Constraints
-- - The path must not make a cycle, otherwise this function loops forever.
-- - There must be a path from the root to the @end@ vertex, otherwise the returned path is not
-- connected to the root.
--
-- @since 1.2.4.0
{-# INLINEABLE constructPathToRoot #-}
constructPathToRoot :: (HasCallStack) => VU.Vector Int -> Int -> VU.Vector Int
constructPathToRoot parents = VU.unfoldr f
  where
    f (-1) = Nothing
    f v = Just (v, parents VG.! v)

-- | \(O(n)\) Given a NxN predecessor matrix (created with `trackingFloydWarshall`), retrieves a
-- path from the root to an end vertex.
--
-- ==== Constraints
-- - The path must not make a cycle, otherwise this function loops forever.
-- - There must be a path from the root to the @end@ vertex, otherwise the returned path is not
-- connected to the root.
--
-- @since 1.2.4.0
{-# INLINE constructPathFromRootMat #-}
constructPathFromRootMat ::
  (HasCallStack) =>
  -- | Predecessor matrix.
  VU.Vector Int ->
  -- | Start vertex.
  Int ->
  -- | End vertex.
  Int ->
  -- | Path.
  VU.Vector Int
constructPathFromRootMat parents start = VU.reverse . constructPathToRootMat parents start

-- | \(O(n)\) Given a NxN predecessor matrix(created with `trackingFloydWarshall`), retrieves a
-- path from a vertex to the root.
--
-- ==== Constraints
-- - The path must not make a cycle, otherwise this function loops forever.
-- - There must be a path from the root to the @end@ vertex, otherwise the returned path is not
-- connected to the root.
--
-- @since 1.2.4.0
{-# INLINEABLE constructPathToRootMat #-}
constructPathToRootMat ::
  (HasCallStack) =>
  -- | Predecessor matrix.
  VU.Vector Int ->
  -- | Start vertex.
  Int ->
  -- | End vertex.
  Int ->
  -- | Path.
  VU.Vector Int
constructPathToRootMat parents start end =
  let parents' = VU.take n $ VU.drop (n * start) parents
   in constructPathToRoot parents' end
  where
    -- Assuming `n < 2^32`, it should always be correct:
    -- https://zenn.dev/mod_poppo/articles/atcoder-beginner-contest-284-d#%E8%A7%A3%E6%B3%953%EF%BC%9Asqrt%E3%81%A8round%E3%82%92%E4%BD%BF%E3%81%86
    n :: Int = round . sqrt $ (fromIntegral (VU.length parents) :: Double)

-- | \(O(n)\) Given a NxN predecessor matrix (created with `newTrackingFloydWarshall`), retrieves a
-- path from the root to an end vertex.
--
-- ==== Constraints
-- - The path must not make a cycle, otherwise this function loops forever.
-- - There must be a path from the root to the @end@ vertex, otherwise the returned path is not
-- connected to the root.
--
-- @since 1.2.4.0
{-# INLINE constructPathFromRootMatM #-}
constructPathFromRootMatM ::
  (HasCallStack, PrimMonad m) =>
  -- | Predecessor matrix.
  VUM.MVector (PrimState m) Int ->
  -- | Start vertex.
  Int ->
  -- | End vertex.
  Int ->
  -- | Path.
  m (VU.Vector Int)
constructPathFromRootMatM parents start = (VU.reverse <$>) . constructPathToRootMatM parents start

-- | \(O(n)\) Given a NxN predecessor matrix (created with `newTrackingFloydWarshall`), retrieves a
-- path from a vertex to the root.
--
-- ==== Constraints
-- - The path must not make a cycle, otherwise this function loops forever.
-- - There must be a path from the root to the @end@ vertex, otherwise the returned path is not
-- connected to the root.
--
-- @since 1.2.4.0
{-# INLINEABLE constructPathToRootMatM #-}
constructPathToRootMatM ::
  (HasCallStack, PrimMonad m) =>
  -- | Predecessor matrix.
  VUM.MVector (PrimState m) Int ->
  -- | Start vertex.
  Int ->
  -- | End vertex.
  Int ->
  -- | Path.
  m (VU.Vector Int)
constructPathToRootMatM parents start end = stToPrim $ do
  parents' <- VU.unsafeFreeze parents
  pure $ constructPathToRootMat parents' start end
