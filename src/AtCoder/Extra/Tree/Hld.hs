{-# LANGUAGE RecordWildCards #-}

-- | Heavy-light decomposition is a technique that breaks down a tree into segments, where each
-- segment is assigned consecutive indices. It allows various path/subtree queries to be processed
-- in \(O(\log n)\) time. See also the @TreeMonoid@ module for integrating segment trees and
-- getting products on paths in \(O(\log^2 n)\) time or subtrees in \(O(\log n)\) time.
--
-- ==== __Overview of the internals__
--
-- ===== Original tree
--
-- Consider a tree with arbitrary vertex order:
--
-- @
--  0--8--7--3--1--2--12--13--15--14     XX: original Vertex
--     |        |                        --: edge
-- 10--5        11--8--6                 |: edge
--     |
--     4
-- @
--
-- ===== `indexHld`: Vertex -> VertexHld
--
-- The tree vertices are reindexed with `indexHld`, where each segment is assigned consecutive
-- vertex indices:
--
-- @
--  0==1==2==3==4==5==6==7==8==9     XX: VertexHld
--     |        |                    ==: edges on the same semgent
-- 14==13       10==11==12           |: edge between different segments
--     |
--     15
-- @
--
-- Note that vertices on higher (shallower) segments are assigned smaller indices. This is very
-- internally very important when calculating `lca`.
--
-- ===== `headHld`: Vertex -> Vertex
--
-- `headHld` points the "head" vertex of each segment. It can be used for finding LCA of two
-- vertices. To find the LCA, move up to the head, go up to the parental segment's vertex and
-- repeat until the two vertices are on the same segment.
--
-- @
--  0==0==0==0==0==0==0==0==0==0     XX: original Vertex
--     |     |
--  5==5      11==11==11
--     |
--     4
-- @
--
-- `headHld` also works for identifying segments. When two vertices are on the same segment, they
-- have the same head.
--
-- ===== `parentHld`: Vertex -> Vertex
--
-- `parentHld` points the parental segment's vertex from a head:
--
-- @
-- (-1)==0==8==7==3==1==2==12==13==15     XX: original Vertex
--       |        |
--    5==8        1==11=8
--       |
--       5
-- @
--
-- ==== __Example__
-- Create an `Hld` for a tree:
--
-- >>> import AtCoder.Extra.Graph qualified as Gr
-- >>> import AtCoder.Extra.Tree.Hld qualified as Hld
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> -- 0--1--2--3
-- >>> --    +
-- >>> --    +--4--5
-- >>> let n = 6
-- >>> let tree = Gr.build' n . Gr.swapDupe' $ VU.fromList [(0, 1), (1, 2), (2, 3), (1, 4), (4, 5)]
-- >>> let hld = Hld.new tree
--
-- `Hld` can process various queries in \(O(\log n)\) time:
--
-- >>> Hld.ancestor hld 5 3 -- go up three parents from `5`
-- 0
--
-- >>> Hld.jump hld 5 2 3   -- go to the third vertex from `5` to `2`:
-- Just 2
--
-- >>> Hld.lengthBetween hld 5 3 -- get the length (the number of edges) between `5` and `3`:
-- 4
--
-- >>> Hld.path hld 5 3     -- get the path between `5` and `3`:
-- [5,4,1,2,3]
--
-- Our `Hld` is rooted at @0@ vertex and subtree queries are available:
--
-- >>> Hld.isInSubtree hld 2 3 -- `3` is in the subtree of `2`
-- True
--
-- >>> Hld.isInSubtree hld 2 4 -- `4` is not in the subtree of `2`
-- False
--
-- Segment queries are used by the @TreeMonoid@ module and s not intended for manual use, here's
-- some examples. This time, the reindex by the HLD is super simple:
--
-- >>> Hld.indexHld hld
-- [0,1,2,3,4,5]
--
-- So we can easily understand the outputs:
--
-- >>> Hld.pathSegmentsInclusive Hld.WeightsAreOnVertices hld 5 3
-- [(5,4),(1,3)]
--
-- >>> Hld.pathSegmentsInclusive Hld.WeightsAreOnEdges hld 5 3 -- LCA (1) is removed
-- [(5,4),(2,3)]
--
-- >>> Hld.subtreeSegmentInclusive hld 1
-- (1,5)
--
-- @since 1.1.0.0
module AtCoder.Extra.Tree.Hld
  ( -- * Hld
    Hld (..),
    Vertex,
    VertexHld,

    -- * Constructors
    new,
    newAt,

    -- * LCA
    lca,

    -- * Jump
    ancestor,
    jump,

    -- * Path
    lengthBetween,
    path,
    pathSegmentsInclusive,

    -- * Subtree
    subtreeSegmentInclusive,
    isInSubtree,

    -- * Products

    -- | These functions are rather for internal use, so see the @TreeMonoid@ module as the primary
    -- API.
    WeightPolicy (..),
    prod,
  )
where

import AtCoder.Extra.Graph qualified as Gr
import AtCoder.Internal.Assert qualified as ACIA
import Control.Monad
import Control.Monad.Fix
import Control.Monad.ST
import Data.Maybe
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Stack (HasCallStack)

-- | Original graph vertex.
--
-- @since 1.1.0.0
type Vertex = Int

-- | Vertex reindexed by `indexHld`.
--
-- @since 1.1.0.0
type VertexHld = Vertex

-- | Hld splits a tree into segments and assignes contiguous `VertexHLD` for each segment.
--
-- @since 1.1.0.0
data Hld = Hld
  { -- | The root vertex.
    --
    -- @since 1.1.0.0
    rootHld :: {-# UNPACK #-} !Vertex,
    -- | Maps `Vertex` to the parent `Vertex`. Returns @-1@ for the root node.
    --
    -- @since 1.1.0.0
    parentHld :: !(VU.Vector Vertex),
    -- | Maps `Vertex` to `VertexHld`, re-indexed vertices contiguous in each segment.
    --
    -- @since 1.1.0.0
    indexHld :: !(VU.Vector VertexHld),
    -- | Maps `Vertex` to the head `Vertex` of the segment.
    --
    -- @since 1.1.0.0
    headHld :: !(VU.Vector Vertex),
    -- | Maps `VertexHld` back to `Vertex`. Used for `ancestor` etc.
    --
    -- @since 1.1.0.0
    revIndexHld :: !(VU.Vector Vertex),
    -- | Maps `Vertex` to their depth from the root. Used for `jump` etc.
    --
    -- @since 1.1.0.0
    depthHld :: !(VU.Vector Int),
    -- | Maps `Vertex` to the subtree size. This is for subtree products.
    --
    -- @since 1.1.0.0
    subtreeSizeHld :: !(VU.Vector Int)
  }
  deriving
    ( -- | @since 1.1.0.0
      Show,
      -- | @since 1.1.0.0
      Eq
    )

-- | \(O(n)\) Creates an `Hld` with \(0\) as the root vertex.
--
-- @since 1.1.0.0
{-# INLINE new #-}
new :: forall w. (HasCallStack) => Gr.Csr w -> Hld
new tree = newAt tree 0

-- | \(O(n)\) Creates an `Hld` with a root vertex specified.
--
-- @since 1.1.0.0
{-# INLINE newAt #-}
newAt :: forall w. (HasCallStack) => Gr.Csr w -> Vertex -> Hld
newAt tree root = runST $ do
  -- Re-create adjacent vertices so that the biggest subtree's head vertex comes first.
  --
  -- We /could/ instead record the biggest adjacent subtree vertex for each vertex, but the other
  -- DFS would be harder.
  let (!tree', !parent, !depths, !subtreeSize) = runST $ do
        adjVec <- VU.thaw (Gr.adjCsr tree)
        parent_ <- VUM.unsafeNew n
        depths_ <- VUM.unsafeNew n
        subtreeSize_ <- VUM.unsafeNew n

        _ <- (\f -> fix f 0 (-1) root) $ \loop depth p v1 -> do
          VGM.write parent_ v1 p
          VGM.write depths_ v1 depth

          (!size1, !eBig) <-
            VU.foldM'
              ( \(!size1, !eBig) (!e2, !v2) -> do
                  if v2 == p
                    then pure (size1, eBig)
                    else do
                      size2 <- loop (depth + 1) v1 v2
                      -- NOTE: It's `>` because we should swap at least once if there's some vertex other
                      -- that the parent_.
                      pure (size1 + size2, if size1 > size2 then eBig else e2)
              )
              (1 :: Int, -1)
              (tree `Gr.eAdj` v1)

          -- move the biggest subtree's head to the first adjacent vertex.
          -- it means the "heavy edge" or the longest segment.
          when (eBig /= -1) $ do
            VGM.swap adjVec eBig $ fst (VG.head (tree `Gr.eAdj` v1))

          -- record subtree size
          VGM.write subtreeSize_ v1 size1

          pure size1

        !vec <- VU.unsafeFreeze adjVec
        (tree {Gr.adjCsr = vec},,,)
          <$> VU.unsafeFreeze parent_
          <*> VU.unsafeFreeze depths_
          <*> VU.unsafeFreeze subtreeSize_

  -- vertex -> reindexed vertex index
  indices <- VUM.replicate n (-1 :: Int)

  -- vertex -> head vertex of the segment
  heads <- VUM.replicate n (-1 :: Int)

  _ <- (\f -> fix f (0 :: Int) root (-1) root) $ \loop acc h p v1 -> do
    -- reindex:
    VGM.write indices v1 acc
    let !acc' = acc + 1

    VGM.write heads v1 h

    -- when the first vertex is within the same segment:
    let (!adj1, !rest) = fromJust $ VU.uncons (tree' `Gr.adj` v1)
    acc'' <-
      if adj1 == p
        then pure acc'
        else loop acc' h v1 adj1

    -- the others are in other segments:
    VU.foldM'
      ( \a v2 -> do
          if v2 == p
            then pure a
            else loop a v2 v1 v2
      )
      acc''
      rest

  !indices' <- VU.unsafeFreeze indices
  let !revIndex = VU.update (VU.replicate n (-1)) $ VU.imap (flip (,)) indices'

  Hld root parent indices'
    <$> VU.unsafeFreeze heads
    <*> pure revIndex
    <*> pure depths
    <*> pure subtreeSize
  where
    !n = Gr.nCsr tree
    !_ = ACIA.runtimeAssert (2 * (Gr.nCsr tree - 1) == Gr.nEdgesCsr tree) "AtCoder.Extra.Hld.newAt: not a non-directed tree"

-- | \(O(\log n)\) Calculates the lowest common ancestor of \(u\) and \(v\).
--
-- @since 1.1.0.0
{-# INLINE lca #-}
lca :: (HasCallStack) => Hld -> Vertex -> Vertex -> Vertex
lca Hld {..} = inner
  where
    inner !x !y
      -- sort for easier processing
      -- TODO: @case compare ix iy@ would be easier for me to understand
      | ix > iy = inner y x
      -- @x@ and @y@ are in other segments:
      | hx /= hy = inner x $ parentHld VG.! hy
      -- @x@ and @y@ are within the same segment:
      -- select the smaller one, which is closer to the root and that is the LCA.
      | otherwise = x
      where
        !ix = indexHld VG.! x
        !iy = indexHld VG.! y
        hx = headHld VG.! x
        hy = headHld VG.! y

-- | \(O(\log n)\) Go up \(k\) times from a vertex \(v\) to the root node. Throws an error if \(k\)
-- is bigger than the depth of \(v\).
--
-- @since 1.1.0.0
{-# INLINE ancestor #-}
ancestor :: (HasCallStack) => Hld -> Vertex -> Int -> Vertex
ancestor Hld {..} parent k0 = inner parent k0
  where
    !_ = ACIA.runtimeAssert (0 <= k0 && k0 <= depthHld VG.! parent) $ "AtCoder.Extra.Tree.Hld.ancestor: k-th ancestor is out of the bounds (`k = " ++ show k0 ++ "`)"
    inner v k
      -- on this segment
      | k <= iv - ihv = revIndexHld VG.! (iv - k)
      -- next segment
      | otherwise = inner (parentHld VG.! hv) (k - (iv - ihv + 1))
      where
        iv = indexHld VG.! v
        hv = headHld VG.! v
        ihv = indexHld VG.! hv

-- | \(O(\log n)\) Returns the \(k\)-th vertex of the path between \(u\) and \(v\) from \(u\).
-- Throws an error if `k` is out
--
-- @since 1.1.0.0
{-# INLINE jump #-}
jump :: (HasCallStack) => Hld -> Vertex -> Vertex -> Int -> Maybe Vertex
jump hld@Hld {..} u v k
  | k > lenU + lenV = Nothing
  | k <= lenU = Just $ ancestor hld u k
  | otherwise = Just $ ancestor hld v (lenU + lenV - k)
  where
    lca_ = lca hld u v
    du = depthHld VG.! u
    dv = depthHld VG.! v
    lenU = du - depthHld VG.! lca_
    lenV = dv - depthHld VG.! lca_

-- | \(O(\log n)\) Returns the length of the path between \(u\) and \(v\).
--
-- @since 1.1.0.0
{-# INLINE lengthBetween #-}
lengthBetween :: (HasCallStack) => Hld -> Vertex -> Vertex -> Int
lengthBetween hld@Hld {..} u v = du - dLca + dv - dLca
  where
    !lca_ = lca hld u v
    !dLca = depthHld VG.! lca_
    !du = depthHld VG.! u
    !dv = depthHld VG.! v

-- | \(O(n)\) Returns the vertices on the path between \(u\) and \(v\).
--
-- @since 1.1.0.0
{-# INLINE path #-}
path :: (HasCallStack) => Hld -> Vertex -> Vertex -> [Vertex]
path hld@Hld {..} u v = concatMap expand $ pathSegmentsInclusive WeightsAreOnVertices hld u v
  where
    expand (!l, !r)
      | l <= r = map (revIndexHld VG.!) [l .. r]
      | otherwise = map (revIndexHld VG.!) [l, l - 1 .. r]

-- | \(O(\log n)\) Decomposes a path between two vertices \(u\) and \(v\) into segments. Each
-- segment is represented as an inclusive range \([u_i, v_i]\) of `VertexHLD`.
--
-- The LCA is omitted from the returning vertices when the weight policy is set to
-- `WeightsAreOnEdges`. This is the trick to put edge weights to on vertices.
--
-- @since 1.1.0.0
{-# INLINE pathSegmentsInclusive #-}
pathSegmentsInclusive :: (HasCallStack) => WeightPolicy -> Hld -> Vertex -> Vertex -> [(VertexHld, VertexHld)]
pathSegmentsInclusive weightPolicy Hld {..} x0 y0 = done $ inner x0 [] y0 []
  where
    isEdge = weightPolicy == WeightsAreOnEdges
    done (!up, !down) = reverse up ++ down
    -- @up@: bottom to top. [(max, min)]
    -- @down@: top to bottom. [(min, max)]
    inner :: Vertex -> [(VertexHld, VertexHld)] -> Vertex -> [(VertexHld, VertexHld)] -> ([(VertexHld, VertexHld)], [(VertexHld, VertexHld)])
    inner x up y down
      | hx == hy && isEdge = case compare ix iy of
          -- skip LCA on edge vertices
          LT -> (up, (ix {- edge -} + 1, iy) : down)
          GT -> ((ix, iy {- edge -} + 1) : up, down)
          EQ -> (up, down)
      | hx == hy && not isEdge = case compare ix iy of
          LT -> (up, (ix, iy) : down)
          _ -> ((ix, iy) : up, down)
      | otherwise = case compare ix iy of
          LT -> inner x up phy ((ihy, iy) : down)
          GT -> inner phx ((ix, ihx) : up) y down
          EQ -> error "unreachable"
      where
        ix, iy :: VertexHld
        !ix = indexHld VG.! x
        !iy = indexHld VG.! y
        hx, hy :: Vertex
        hx = headHld VG.! x
        hy = headHld VG.! y
        ihx, ihy :: VertexHld
        ihx = indexHld VG.! hx
        ihy = indexHld VG.! hy
        phx, phy :: VertexHld
        phx = parentHld VG.! hx
        phy = parentHld VG.! hy

-- | \(O(1)\) Returns a half-open interval of `VertexHld` \([\mathrm{start}, \mathrm{end})\) that
-- corresponds to the subtree segments rooted at the given @subtreeRoot@.
--
-- @since 1.1.0.0
{-# INLINE subtreeSegmentInclusive #-}
subtreeSegmentInclusive :: (HasCallStack) => Hld -> Vertex -> (VertexHld, VertexHld)
subtreeSegmentInclusive Hld {..} subtreeRoot = (ir, ir + sr - 1)
  where
    ir = indexHld VG.! subtreeRoot
    sr = subtreeSizeHld VG.! subtreeRoot

-- | \(O(1)\) Returns `True` if \(u\) is in a subtree of \(r\).
--
-- @since 1.1.0.0
{-# INLINE isInSubtree #-}
isInSubtree :: (HasCallStack) => Hld -> Vertex -> Vertex -> Bool
isInSubtree hld@Hld {..} r_ u = l <= iu && iu <= r
  where
    (!l, !r) = subtreeSegmentInclusive hld r_
    !iu = indexHld VG.! u

-- | Represents whether weights are put on vertices or edges.
--
-- @since 1.1.0.0
data WeightPolicy
  = -- | Weights are put on vertices.
    --
    -- @since 1.1.0.0
    WeightsAreOnVertices
  | -- | Weights are put on edges.
    --
    -- @since 1.1.0.0
    WeightsAreOnEdges
  deriving
    ( -- | @since 1.1.0.0
      Eq,
      -- | @since 1.1.0.0
      Show
    )

-- | \(O(\log n f)\) Returns product of the path between \(u\) and \(v\), using the user functions
-- of time complexity \(O(f)\).
--
-- @since 1.1.0.0
{-# INLINE prod #-}
prod ::
  (HasCallStack, Monoid mono, Monad m) =>
  -- | The `WeightPolicy`.
  WeightPolicy ->
  -- | The `Hld`.
  Hld ->
  -- | User function for getting products in \([u, v)\), where \(u < v\) and
  -- \(\mathrm{depth}(u) < \mathrm{depth}(v)\).
  (VertexHld -> VertexHld -> m mono) ->
  -- | User function for getting products in \([u, v)\), where \(u < v\) and
  -- \(\mathrm{depth}(u) > \mathrm{depth}(v)\).
  (VertexHld -> VertexHld -> m mono) ->
  -- | \(u\).
  Vertex ->
  -- | \(v\).
  Vertex ->
  -- | Product of the path between \(u\) and \(v\).
  m mono
prod weightPolicy hld prodF prodB u0 v0 = do
  foldM
    ( \ !acc (!u, !v) -> do
        !x <-
          if u <= v
            then prodF u $ v + 1
            else prodB v $ u + 1
        pure $! acc <> x
    )
    mempty
    (pathSegmentsInclusive weightPolicy hld u0 v0)
