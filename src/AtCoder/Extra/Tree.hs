-- | Generic tree functions.
--
-- @since 1.1.0.0
module AtCoder.Extra.Tree
  ( -- * Tree properties
    diameter,
    diameterPath,

    -- * Minimum spanning tree
    mst,
    mstBy,

    -- * Tree folding

    -- | These functions are built around the three type parameters: \(w\), \(f\), and \(a\).
    --
    -- - \(w\): Edge weight.
    -- - \(f\): Monoid action to a vertex value. These actions are created from vertex value \(a\)
    -- and edge information @(Int, w)@.
    -- - \(a\): Monoid values stored at vertices.
    fold,
    scan,
    foldReroot,
  )
where

import AtCoder.Dsu qualified as Dsu
import AtCoder.Extra.Graph qualified as Gr
import Control.Monad (when)
import Control.Monad.ST (runST)
import Data.Bit (Bit (..))
import Data.Functor.Identity (runIdentity)
import Data.Maybe (isJust)
import Data.Ord (comparing)
import Data.Vector.Algorithms.Intro qualified as VAI
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Stack (HasCallStack)

-- | \(O(n + m)\) Returns the endpoints of the diameter of a tree and their distance: \(((u, v), w)\).
--
-- ==== __Example__
-- >>> import AtCoder.Extra.Graph qualified as Gr
-- >>> import AtCoder.Extra.Tree qualified as Tree
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> let es = VU.fromList [(0, 1, 1 :: Int), (1, 2, 10), (1, 3, 10)]
-- >>> let gr = Gr.build 4 $ Gr.swapDupe es
-- >>> Tree.diameter 4 (Gr.adjW gr) (-1)
-- ((2,3),20)
--
-- @since 1.2.4.0
{-# INLINEABLE diameter #-}
diameter ::
  (HasCallStack, VU.Unbox w, Num w, Ord w) =>
  -- | The number of vertices.
  Int ->
  -- | Graph given as a function.
  (Int -> VU.Vector (Int, w)) ->
  -- | Distances assigned to unreachable vertices.
  w ->
  -- | Tuple of (endpoints of the longest path in a tree, distance of it).
  ((Int, Int), w)
diameter n gr !undefW =
  let !bfs1 = Gr.bfs n gr undefW $ VU.singleton (0, 0)
      !from = VU.maxIndex bfs1
      !bfs2 = Gr.bfs n gr undefW $ VU.singleton (from, 0)
      !to = VU.maxIndex bfs2
      !w = VU.maximum bfs2
   in ((from, to), w)

-- | \(O(n + m)\) Returns the longest path in a tree and the distance of it.
--
-- ==== __Example__
-- >>> import AtCoder.Extra.Graph qualified as Gr
-- >>> import AtCoder.Extra.Tree qualified as Tree
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> let es = VU.fromList [(0, 1, 1 :: Int), (1, 2, 10), (1, 3, 10)]
-- >>> let gr = Gr.build 4 $ Gr.swapDupe es
-- >>> Tree.diameterPath 4 (Gr.adjW gr) (-1)
-- ([2,1,3],20)
--
-- @since 1.2.4.0
{-# INLINEABLE diameterPath #-}
diameterPath ::
  (HasCallStack, Show w, VU.Unbox w, Num w, Ord w) =>
  -- | The number of vertices.
  Int ->
  -- | Graph given as a function.
  (Int -> VU.Vector (Int, w)) ->
  -- | Distances assigned to unreachable vertices.
  w ->
  -- | Tuple of (the longest path, distance of it).
  (VU.Vector Int, w)
diameterPath n gr !undefW =
  let !bfs1 = Gr.bfs n gr undefW $ VU.singleton (0, 0)
      !from = VU.maxIndex bfs1
      (!bfs2, !parents) = Gr.trackingBfs n gr undefW $ VU.singleton (from, 0)
      !to = VU.maxIndex bfs2
      !w = bfs2 VG.! to
   in (Gr.constructPathFromRoot parents to, w)

-- | \(O(m \log m)\) Kruskal's algorithm. Returns edge indices for building a minimum spanning tree.
--
-- NOTE: The edges should not be duplicated: only one of \((u, v, w)\) or \((v, u, w)\) is required
-- for each edge.
--
-- ==== __Example__
-- Create a minimum spanning tree:
--
-- >>> import AtCoder.Extra.Tree qualified as Tree
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> let es = VU.fromList [(0, 1, 1 :: Int), (1, 2, 10), (0, 2, 2)]
-- >>> let (!wSum, !edgeUse, !gr) = Tree.mst 3 es
-- >>> wSum
-- 3
--
-- >>> edgeUse
-- [1,0,1]
--
-- >>> Gr.adj gr 0
-- [1,2]
--
-- @since 1.2.4.0
{-# INLINE mst #-}
mst :: (Num w, Ord w, VU.Unbox w) => Int -> VU.Vector (Int, Int, w) -> (w, VU.Vector Bit, Gr.Csr w)
mst = mstBy (comparing id)

-- | \(O(m \log m)\) Kruskal's algorithm. Returns edge indices for building a minimum/maximum
-- spanning tree.
--
-- NOTE: The edges should not be duplicated: only one of \((u, v, w)\) or \((v, u, w)\) is required
-- for each edge.
--
-- ==== __Example__
-- Create a maximum spanning tree:
--
-- >>> import AtCoder.Extra.Tree qualified as Tree
-- >>> import Data.Ord (Down (..))
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> let es = VU.fromList [(0, 1, 1 :: Int), (1, 2, 10), (0, 2, 2)]
-- >>> let (!wSum, !edgeUse, !gr) = Tree.mstBy (comparing Down) 3 es
-- >>> wSum
-- 12
--
-- >>> edgeUse
-- [0,1,1]
--
-- >>> Gr.adj gr 0
-- [2]
--
-- @since 1.2.4.0
{-# INLINEABLE mstBy #-}
mstBy :: (Num w, Ord w, VU.Unbox w) => (w -> w -> Ordering) -> Int -> VU.Vector (Int, Int, w) -> (w, VU.Vector Bit, Gr.Csr w)
mstBy !f nVerts edges = runST $ do
  dsu <- Dsu.new nVerts
  wSum <- VUM.replicate 1 0
  use <-
    ( VU.accumulate
        (const id)
        (VU.replicate (VU.length edges) (Bit False))
        <$>
      )
      . VU.mapM
        ( \(i :: Int) -> do
            let !u = us VG.! i
            let !v = vs VG.! i
            b <- isJust <$> Dsu.mergeMaybe dsu u v
            when b $ do
              VGM.modify wSum (+ ws VG.! i) 0
            pure (i, Bit b)
        )
      . VU.modify (VAI.sortBy (\(i :: Int) (j :: Int) -> f (ws VG.! i) (ws VG.! j)))
      $ VU.generate (VU.length edges) id
  let !gr = Gr.build nVerts $ Gr.swapDupe $ VU.ifilter (\i _ -> unBit (use VG.! i)) edges
  (,use,gr) <$> VGM.read wSum 0
  where
    (!us, !vs, !ws) = VU.unzip3 edges

{-# INLINEABLE foldImpl #-}
foldImpl ::
  forall m w f a.
  (HasCallStack, Monad m, VU.Unbox w) =>
  (Int -> VU.Vector (Int, w)) ->
  (Int -> a) ->
  (a -> (Int, w) -> f) ->
  (f -> a -> a) ->
  Int ->
  (Int -> a -> m ()) ->
  m a
foldImpl tree valAt toF act root memo = inner (-1) root
  where
    inner :: Int -> Int -> m a
    inner !parent !v1 = do
      let !acc0 = valAt v1
      let !v2s = VU.filter ((/= parent) . fst) $ tree v1
      !res <- VU.foldM' (\acc (!v2, !w) -> (`act` acc) . (`toF` (v1, w)) <$> inner v1 v2) acc0 v2s
      memo v1 res
      pure res

-- | \(O(n)\) Folds a tree from a root vertex, also known as tree DP.
--
-- ==== __Example__
-- >>> import AtCoder.Extra.Graph qualified as Gr
-- >>> import AtCoder.Extra.Tree qualified as Tree
-- >>> import Data.Semigroup (Sum (..))
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> let gr = Gr.build @(Sum Int) 5 . Gr.swapDupe $ VU.fromList [(2, 1, Sum 1), (1, 0, Sum 1), (2, 3, Sum 1), (3, 4, Sum 1)]
-- >>> type W = Sum Int -- edge weight
-- >>> type F = Sum Int -- action type
-- >>> type X = Sum Int -- vertex value
-- >>> :{
--  let res = Tree.fold (gr `Gr.adjW`) valAt toF act 2
--        where
--          valAt :: Int -> X
--          valAt = const $ mempty @(Sum Int)
--          toF :: X -> (Int, W) -> F
--          toF x (!_i, !dx) = x + dx
--          act :: F -> X -> X
--          act dx x = dx + x
--   in getSum res
-- :}
-- 4
--
-- @since 1.1.0.0
{-# INLINE fold #-}
fold ::
  (HasCallStack, VU.Unbox w) =>
  -- | Graph as a function.
  (Int -> VU.Vector (Int, w)) ->
  -- | @valAt@: Assignment of initial vertex values.
  (Int -> a) ->
  -- | @toF@: Converts a vertex value into an action onto a neighbor vertex.
  (a -> (Int, w) -> f) ->
  -- | @act@: Performs an action onto a vertex value.
  (f -> a -> a) ->
  -- | Root vertex.
  Int ->
  -- | Tree folding result from the root vertex.
  a
fold tree valAt toF act root = runIdentity $ do
  foldImpl tree valAt toF act root (\_ _ -> pure ())

-- | \(O(n)\) Folds a tree from a root vertex, also known as tree DP. The calculation process on
-- every vertex is recoreded and returned as a vector.
--
-- ==== __Example__
-- >>> import AtCoder.Extra.Graph qualified as Gr
-- >>> import AtCoder.Extra.Tree qualified as Tree
-- >>> import Data.Semigroup (Sum (..))
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> let n = 5
-- >>> let gr = Gr.build @(Sum Int) n . Gr.swapDupe $ VU.fromList [(2, 1, Sum 1), (1, 0, Sum 1), (2, 3, Sum 1), (3, 4, Sum 1)]
-- >>> type W = Sum Int -- edge weight
-- >>> type F = Sum Int -- action type
-- >>> type X = Sum Int -- vertex value
-- >>> :{
--  let res = Tree.scan n (gr `Gr.adjW`) valAt toF act 2
--        where
--          valAt :: Int -> X
--          valAt = const $ mempty @(Sum Int)
--          toF :: X -> (Int, W) -> F
--          toF x (!_i, !dx) = x + dx
--          act :: F -> X -> X
--          act dx x = dx + x
--   in VU.map getSum res
-- :}
-- [0,1,4,1,0]
--
-- @since 1.3.0.0
{-# INLINE scan #-}
scan ::
  (VU.Unbox w, VU.Unbox a) =>
  -- | The number of vertices.
  Int ->
  -- | Graph as a function.
  (Int -> VU.Vector (Int, w)) ->
  -- | @valAt@: Assignment of initial vertex values.
  (Int -> a) ->
  -- | @toF@: Converts a vertex value into an action onto a neighbor vertex.
  (a -> (Int, w) -> f) ->
  -- | @act@: Performs an action onto a vertex value.
  (f -> a -> a) ->
  -- | Root vertex.
  Int ->
  -- | Tree scanning result from a root vertex.
  VU.Vector a
scan n tree acc0At toF act root = VU.create $ do
  dp <- VUM.unsafeNew n
  !_ <- foldImpl tree acc0At toF act root $ \v a -> do
    VGM.unsafeWrite dp v a
  pure dp

-- | \(O(n)\) Folds a tree from every vertex, using the rerooting technique.
--
-- ==== Constraints
-- - The action monoid \(f\) must be commutative.
--
-- ==== __Example__
-- >>> import AtCoder.Extra.Graph qualified as Gr
-- >>> import AtCoder.Extra.Tree qualified as Tree
-- >>> import Data.Semigroup (Sum (..))
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> let n = 5
-- >>> let gr = Gr.build @(Sum Int) n . Gr.swapDupe $ VU.fromList [(2, 1, Sum 1), (1, 0, Sum 1), (2, 3, Sum 1), (3, 4, Sum 1)]
-- >>> type W = Sum Int -- edge weight
-- >>> type F = Sum Int -- action type
-- >>> type X = Sum Int -- vertex value
-- >>> :{
--  let res = Tree.foldReroot n (gr `Gr.adjW`) valAt toF act
--        where
--          valAt :: Int -> X
--          valAt = const $ mempty @(Sum Int)
--          toF :: X -> (Int, W) -> F
--          toF x (!_i, !dx) = x + dx
--          act :: F -> X -> X
--          act dx x = dx + x
--   in VU.map getSum res
-- :}
-- [4,4,4,4,4]
--
-- @since 1.1.0.0
{-# INLINEABLE foldReroot #-}
foldReroot ::
  forall w f a.
  (HasCallStack, VU.Unbox w, VU.Unbox a, VU.Unbox f, Monoid f) =>
  -- | The number of vertices.
  Int ->
  -- | Graph as a function.
  (Int -> VU.Vector (Int, w)) ->
  -- | @valAt@:Assignment of initial vertex values.
  (Int -> a) ->
  -- | @toF@: Converts a vertex value into an action onto a neighbor vertex.
  (a -> (Int, w) -> f) ->
  -- | @act@: Performs an action onto a vertex value.
  (f -> a -> a) ->
  -- | Tree folding result from every vertex as a root.
  VU.Vector a
foldReroot n tree valAt toF act = VU.create $ do
  -- Calculate tree DP for every vertex as a root:
  !dp <- VUM.unsafeNew n
  let reroot parent parentF v1 = do
        let !children = VU.filter ((/= parent) . fst) $ tree v1
        let !fL = VU.scanl' (\ !f (!v2, !w) -> (f <>) . (`toF` (v1, w)) $ treeDp VG.! v2) f0 children
        let !fR = VU.scanr' (\(!v2, !w) !f -> (<> f) . (`toF` (v1, w)) $ treeDp VG.! v2) f0 children

        -- save
        let !x1 = (parentF <> VU.last fL) `act` valAt v1
        VGM.unsafeWrite dp v1 x1

        VU.iforM_ children $ \i2 (!v2, !w) -> do
          -- composited operator excluding @v2@:
          let !f1 = parentF <> (fL VG.! i2) <> (fR VG.! (i2 + 1))
          let !v1Acc = f1 `act` valAt v1
          let !f2 = toF v1Acc (v2, w)
          reroot v1 f2 v2

  reroot (-1 :: Int) f0 root0
  pure dp
  where
    !root0 = 0 :: Int
    !f0 = mempty @f
    !treeDp = scan n tree valAt toF act root0 :: VU.Vector a
