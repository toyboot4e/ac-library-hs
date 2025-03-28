{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- | Re-export of the @Csr@ module and generic graph search functions.
--
-- @since 1.1.0.0
module AtCoder.Extra.Graph
  ( -- * Re-export of CSR

    -- | The `Csr.Csr` data type and all the functions such as `build` or `adj` are re-exported.
    module Csr,

    -- * CSR helpers
    swapDupe,
    swapDupe',
    scc,
    rev,

    -- * Generic graph functions
    topSort,
    blockCut,
    blockCutComponents,

    -- * Shortest path search functions (opinionated)

    -- ** BFS
    bfs,
    trackingBfs,
    bfs01,

    -- ** Path reconstruction
    constructPathFromRoot,
    constructPathToRoot,
  )
where

import AtCoder.Extra.IntSet qualified as IS
import AtCoder.Extra.Ix0 (Bounds0, Ix0 (..))
import AtCoder.Internal.Buffer qualified as B
import AtCoder.Internal.Csr as Csr
import AtCoder.Internal.Queue qualified as Q
import AtCoder.Internal.Scc qualified as ACISCC
import Control.Monad (when)
import Control.Monad.Fix (fix)
import Control.Monad.ST (runST)
import Data.Bit (Bit (..))
import Data.Foldable (for_)
import Data.Maybe (fromJust)
import Data.Vector qualified as V
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Stack (HasCallStack)

-- | \(O(n)\) Converts directed edges into non-directed edges. This is a convenient function for
-- making an input to `build`.
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
{-# INLINE swapDupe #-}
swapDupe :: (VU.Unbox w) => VU.Vector (Int, Int, w) -> VU.Vector (Int, Int, w)
swapDupe = VU.concatMap (\(!u, !v, !w) -> VU.fromListN 2 [(u, v, w), (v, u, w)])

-- | \(O(n)\) Converts directed edges into non-directed edges. This is a convenient function for
-- making an input to `build'`.
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
{-# INLINE swapDupe' #-}
swapDupe' :: VU.Vector (Int, Int) -> VU.Vector (Int, Int)
swapDupe' = VU.concatMap (\(!u, !v) -> VU.fromListN 2 [(u, v), (v, u)])

-- | \(O(n + m)\) Returns the strongly connected components.
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
-- \((v, u, w)\).
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
-- - The graph must be a DAG.
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

-- | \(O(n + m)\) Returns a [block cut tree](https://en.wikipedia.org/wiki/Biconnected_component)
-- where super vertices represent each biconnected component.
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
                          for_ [1 .. len - s] $ \_ -> do
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

-- | \(O(n + m)\) Returns a [blocks (biconnected comopnents)](https://en.wikipedia.org/wiki/Biconnected_component)
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
{-# INLINE blockCutComponents #-}
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
-- @since 1.2.4.0
{-# INLINEABLE bfs #-}
bfs ::
  forall i w.
  (HasCallStack, Ix0 i, VU.Unbox i, VU.Unbox w, Num w, Eq w) =>
  -- | Boundary
  Bounds0 i ->
  -- | Graph function that takes a vertex and their distance and returns adjacent vertices with
  -- edge weights, where \(w > 0\)
  (i -> w -> VU.Vector (i, w)) ->
  -- | Distance assignment for unreachable vertices.
  w ->
  -- | Weighted source vertices
  VU.Vector (i, w) ->
  -- | Distance array in one-dimensional index. Unreachable vertices are given distance of @undefW@.
  VU.Vector w
bfs bnd0 gr undefW sources
  | VU.null sources = VU.replicate nVerts (-1)
  | otherwise = VU.create $ do
      dist <- VUM.replicate @_ @w nVerts undefW
      -- NOTE: We only need capacity of `n`, as first appearance of vertex is with minimum distance.
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
            !d1 <- VGM.read dist $! index0 bnd0 v1
            VU.forM_ (gr v1 d1) $ \(!v2, !dw) -> do
              let !i2 = index0 bnd0 v2
              !lastD <- VGM.read dist i2
              when (lastD == undefW) $ do
                VGM.write dist i2 $! d1 + dw
                Q.pushBack queue v2
            loop

      pure dist
  where
    !nVerts = rangeSize0 bnd0

-- | \(O(n + m)\) Opinionated breadth-first search that returns a distance array and a predecessor
-- array.
--
-- @since 1.2.4.0
{-# INLINEABLE trackingBfs #-}
trackingBfs ::
  forall i w.
  (HasCallStack, Ix0 i, VU.Unbox i, VU.Unbox w, Num w, Eq w) =>
  -- | Boundary
  Bounds0 i ->
  -- | Graph function that takes a vertex and their distance and returns adjacent vertices with
  -- edge weights, where \(w > 0\)
  (i -> w -> VU.Vector (i, w)) ->
  -- | Distance assignment for unreachable vertices.
  w ->
  -- | Weighted source vertices
  VU.Vector (i, w) ->
  -- | (Distance vector in one-dimensional index, Predecessor array (@-1@ represents none))
  (VU.Vector w, VU.Vector Int)
trackingBfs bnd0 gr undefW sources
  | VU.null sources = (VU.replicate nVerts undefW, VU.replicate nVerts (-1))
  | otherwise = runST $ do
      dist <- VUM.replicate @_ @w nVerts undefW
      prev <- VUM.replicate @_ @Int nVerts (-1)
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
          -- VGM.write prev i (-1)
          Q.pushBack queue src

      -- run BFS
      fix $ \loop -> do
        Q.popFront queue >>= \case
          Nothing -> pure ()
          Just v1 -> do
            let !i1 = index0 bnd0 v1
            !d1 <- VGM.read dist i1
            VU.forM_ (gr v1 d1) $ \(!v2, !dw) -> do
              let !i2 = index0 bnd0 v2
              !lastD <- VGM.read dist i2
              when (lastD == undefW) $ do
                VGM.write dist i2 $! d1 + dw
                VGM.write prev i2 i1
                Q.pushBack queue v2
            loop

      (,) <$> VU.unsafeFreeze dist <*> VU.unsafeFreeze prev
  where
    !nVerts = rangeSize0 bnd0

-- | \(O(n + m)\) Opinionated 01-BFS that returns a distance array.
--
-- @since 1.2.4.0
{-# INLINEABLE bfs01 #-}
bfs01 ::
  forall i.
  (HasCallStack, Ix0 i, VU.Unbox i) =>
  -- | Zero-based index boundary.
  Bounds0 i ->
  -- | Graph function that takes the vertex, current distance and returns adjacent vertices with
  -- edge weights, where \(w > 0\)
  (i -> Int -> VU.Vector (i, Int)) ->
  -- | Capacity of deque, often the number of edges.
  Int ->
  -- | Distance assignment for unreachable vertices.
  VU.Vector (i, Int) ->
  -- | Distance array in one-dimensional index. Unreachable vertices are given distance of `-1`.
  VU.Vector Int
bfs01 !bnd0 !gr !capactiy !sources
  | VU.null sources = VU.replicate nVerts (-1)
  | otherwise = VU.create $ do
      dist <- VUM.replicate nVerts undef
      -- NOTE: Just like Dijkstra, we need capacity of `m`, as the first appearance of a vertex is not
      -- always with minimum distance.
      deque <- Q.newDeque @_ @(i, Int) (capactiy + 1)

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
              VU.forM_ (gr vExt0 w0) $ \(!vExt, !dw) -> do
                let !w = w0 + dw
                let !i = index0 bnd0 vExt
                !wReserved <- VGM.read dist i
                -- NOTE: Do pruning just like Dijkstra:
                when (wReserved == undef || w < wReserved) $ do
                  VGM.write dist i w
                  if dw == 0
                    then Q.pushFront deque (vExt, w)
                    else Q.pushBack deque (vExt, w)

      fix $ \popLoop -> do
        Q.popFront deque >>= \case
          Nothing -> pure ()
          Just (!vExt0, !w0) -> do
            step vExt0 w0
            popLoop

      pure dist
  where
    !undef = -1 :: Int
    !nVerts = rangeSize0 bnd0

-- | \(O(n)\) Given a predecessor array, retrieves a path from the root to a vertex.
--
-- @since 1.2.4.0
constructPathFromRoot :: (HasCallStack) => VU.Vector Int -> Int -> VU.Vector Int
constructPathFromRoot parents = VU.reverse . constructPathToRoot parents

-- | \(O(n)\) Given a predecessor array, retrieves a path from a vertex to the root.
--
-- @since 1.2.4.0
constructPathToRoot :: (HasCallStack) => VU.Vector Int -> Int -> VU.Vector Int
constructPathToRoot parents = VU.unfoldr f
  where
    f (-1) = Nothing
    f v = Just (v, parents VG.! v)
