{-# LANGUAGE RecordWildCards #-}

-- | Integration of segment trees with the heavy-light decomposition technique. Computes monoid
-- products on a path in \(O(\log^2 n)\) time or on a subtree in \(O(\log n)\) time.
--
-- - If vertices have weights, create a `TreeMonoid` with `fromVerts`.
-- - If edges have weights, create a tree monoid with `fromEdges`.
--
-- ==== __(Internals) Weights on edges__
--
-- When vertices are unweighted and only edges have weights, treat edges as new vertices or assign
-- edge weights to the deeper vertex.
--
-- Idea 1. Convert edges into new vertices. This is inefficient.
--
-- @
-- o--o--o  --> o-x-o-x-o
-- @
--
-- Idea 2. Assign edge weight to the deeper vertex. This is the internal implementation of
-- `fromEdges` and LCAs are ignored on `prod`:
--
-- @
--   o
--   | <--- edge 1
--   o <- write weight 1 here
--   | <--- edge 2
--   o <- write weight 2 here
-- @
--
-- ==== __Example (1): Weights are on vertices__
-- >>> import AtCoder.Extra.Graph qualified as Gr
-- >>> import AtCoder.Extra.Tree.Hld qualified as Hld
-- >>> import AtCoder.Extra.Tree.TreeMonoid qualified as TM
-- >>> import Data.Semigroup (Sum (..))
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> -- 0--1--2--3
-- >>> --    +
-- >>> --    +--4--5
-- >>> let n = 6
-- >>> -- note that the edges must be bi-directed:
-- >>> let tree = Gr.build' n . Gr.swapDupe' $ VU.fromList [(0, 1), (1, 2), (2, 3), (1, 4), (4, 5)]
-- >>> let weights = VU.generate n Sum -- vertex `i` is given weight of `i`
-- >>> let hld = Hld.new tree
-- >>> tm <- TM.fromVerts hld {- `Sum` is commutative -} Commute weights
-- >>> TM.prod tm 1 3
-- Sum {getSum = 6}
--
-- >>> TM.prodSubtree tm 1
-- Sum {getSum = 15}
--
-- >>> TM.write tm 1 $ Sum 10
-- >>> TM.prod tm 1 3
-- Sum {getSum = 15}
--
-- ==== __Example (2): Weights are on edges__
-- >>> import AtCoder.Extra.Graph qualified as Gr
-- >>> import AtCoder.Extra.Tree.Hld qualified as Hld
-- >>> import AtCoder.Extra.Tree.TreeMonoid qualified as TM
-- >>> import Data.Semigroup (Sum (..))
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> -- 0--1--2--3
-- >>> --    +
-- >>> --    +--4--5
-- >>> let n = 6
-- >>> let edges = VU.fromList [(0, 1, Sum (1 :: Int)), (1, 2, Sum 2), (2, 3, Sum 3), (1, 4, Sum 4), (4, 5, Sum 5)]
-- >>> -- note that the edges must be bi-directed:
-- >>> let tree = Gr.build n $ Gr.swapDupe edges
-- >>> let hld = Hld.new tree
-- >>> -- note that the edge doesn't have to be bi-directed:
-- >>> tm <- TM.fromEdges hld {- `Sum` is commutative -} Commute edges
-- >>> TM.prod tm 1 3
-- Sum {getSum = 5}
--
-- >>> TM.prodSubtree tm 1
-- Sum {getSum = 14}
--
-- >>> TM.write tm 2 $ Sum 10
-- >>> TM.prod tm 1 3
-- Sum {getSum = 13}
--
-- @since 1.1.0.0
module AtCoder.Extra.Tree.TreeMonoid
  ( -- * TreeMonoid
    TreeMonoid,
    Vertex,
    VertexHld,
    Commutativity (..),

    -- * Constructors
    fromVerts,
    fromEdges,

    -- * Segment tree methods

    -- ** Reading
    prod,
    prodSubtree,
    read,

    -- ** Modifications
    write,
    exchange,
    modify,
    modifyM,
  )
where

import AtCoder.Extra.Tree.Hld qualified as Hld
import AtCoder.Internal.Assert qualified as ACIA
import AtCoder.SegTree qualified as ST
import Control.Monad
import Control.Monad.Primitive (PrimMonad, PrimState, stToPrim)
import Control.Monad.ST (ST)
import Data.Monoid (Dual (..))
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Stack (HasCallStack)
import Prelude hiding (read)

-- | Original graph vertex.
--
-- @since 1.1.0.0
type Vertex = Int

-- | Vertex reindexed by `indexHld`.
--
-- @since 1.1.0.0
type VertexHld = Vertex

-- | A wrapper of `Hld` for getting monoid product on paths on a tree using `Hld` and segment
-- tree(s).
--
-- @since 1.1.0.0
data TreeMonoid a s = TreeMonoid
  { -- | Borrowed Hld.
    hldTM :: !Hld.Hld,
    -- | Indicates if it's targetting commutative monoids.
    commuteTM :: !Commutativity,
    -- | Indicates if it's targetting edge weights (If not, it's targetting vertex weights).
    weightPolicyTM :: !Hld.WeightPolicy,
    -- | Segment tree for getting products upwards.
    segFTM :: !(ST.SegTree s a),
    -- | Segment tree for getting products downwards. Only created when the monoid is
    -- `NonCommute`.
    segBTM :: !(ST.SegTree s (Dual a))
  }

-- | Represents whether a monoid is commutative or noncommutative.
--
-- @since 1.1.0.0
data Commutativity
  = -- | Commutative: \(a \cdot b = b \cdot a\).
    --
    -- @since 1.1.0.0
    Commute
  | -- | Noncommutative: \(a \cdot b \neq b \cdot a\).
    --
    -- @since 1.1.0.0
    NonCommute
  deriving
    ( -- | @since 1.1.0.0
      Eq,
      -- | @since 1.1.0.0
      Show
    )

-- | \(O(n)\) Creates a `TreeMonoid` with weights on vertices.
--
-- @since 1.1.0.0
{-# INLINE fromVerts #-}
fromVerts ::
  (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) =>
  -- | `Hld.Hld`.
  Hld.Hld ->
  -- | `Commutativity` of the monoid.
  Commutativity ->
  -- | The vertex weights.
  VU.Vector a ->
  -- | A `TreeMonoid` with weights on vertices.
  m (TreeMonoid a (PrimState m))
fromVerts hld commuteTM xs_ = stToPrim $ fromVertsST hld commuteTM xs_

-- | \(O(n)\) Creates a `TreeMonoid` with weignts on edges. The don't have to be bi-directed: only
-- one of \((u, v, w)\) or \((v, u, w)\) is needed.
--
-- @since 1.1.0.0
{-# INLINE fromEdges #-}
fromEdges ::
  (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) =>
  -- | `Hld.Hld`.
  Hld.Hld ->
  -- | `Commutativity` of the monoid.
  Commutativity ->
  -- | Input edges.
  VU.Vector (Vertex, Vertex, a) ->
  -- | A `TreeMonoid` with weights on edges.
  m (TreeMonoid a (PrimState m))
fromEdges hld commuteTM edges = stToPrim $ fromEdgesST hld commuteTM edges

-- | \(O(\log^2 n)\) Returns the monoid product of the path between two vertices \(u\) and \(v\)
-- (invlusive).
--
-- @since 1.1.0.0
{-# INLINE prod #-}
prod :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => TreeMonoid a (PrimState m) -> Vertex -> Vertex -> m a
prod tm u v = stToPrim $ prodST tm u v

-- | \(O(\log n)\) Returns the product of the subtree rooted at the given `Vertex`.
--
-- @since 1.1.0.0
{-# INLINE prodSubtree #-}
prodSubtree :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => TreeMonoid a (PrimState m) -> Vertex -> m a
prodSubtree tm subtreeRoot = stToPrim $ prodSubtreeST tm subtreeRoot

-- | \(O(1)\) Reads a monoid value of a `Vertex`.
--
-- @since 1.1.0.0
{-# INLINE read #-}
read :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => TreeMonoid a (PrimState m) -> Vertex -> m a
read tm i_ = stToPrim $ readST tm i_

-- | \(O(\log n)\) Writes to the monoid value of a vertex.
--
-- @since 1.1.0.0
{-# INLINE write #-}
write :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => TreeMonoid a (PrimState m) -> Vertex -> a -> m ()
write tm i_ x = stToPrim $ writeST tm i_ x

-- | \(O(\log n)\) Writes to the monoid value of a vertex and returns the old value.
--
-- @since 1.1.0.0
{-# INLINE exchange #-}
exchange :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => TreeMonoid a (PrimState m) -> Vertex -> a -> m a
exchange tm i_ x = stToPrim $ exchangeST tm i_ x

-- | \(O(\log n)\) Given a user function \(f\), modifies the monoid value at \(v\).
--
-- @since 1.1.0.0
{-# INLINE modify #-}
modify :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => TreeMonoid a (PrimState m) -> (a -> a) -> Int -> m ()
modify tm f i_ = stToPrim $ modifyST tm f i_

-- | \(O(\log n)\) Given a user function \(f\), modifies the monoid value at \(v\).
--
-- @since 1.1.0.0
{-# INLINE modifyM #-}
modifyM :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => TreeMonoid a (PrimState m) -> (a -> m a) -> Int -> m ()
modifyM TreeMonoid {..} f i_ = do
  let !i = Hld.indexHld hldTM VG.! i_
  ST.modifyM segFTM f i
  when (commuteTM == NonCommute) $ do
    ST.modifyM segBTM ((Dual <$>) . f . getDual) i

-- -------------------------------------------------------------------------------------------------
-- INLINE
-- -------------------------------------------------------------------------------------------------

{-# INLINEABLE buildST #-}
buildST ::
  (HasCallStack, Monoid a, VU.Unbox a) =>
  Hld.Hld ->
  Commutativity ->
  Hld.WeightPolicy ->
  VU.Vector a ->
  ST s (TreeMonoid a s)
buildST hldTM commuteTM weightPolicyTM weights = do
  segFTM <- ST.build weights
  segBTM <-
    case commuteTM of
      Commute -> ST.build VU.empty
      NonCommute -> ST.build $ VU.map Dual weights
  pure TreeMonoid {..}

{-# INLINEABLE fromVertsST #-}
fromVertsST ::
  (HasCallStack, Monoid a, VU.Unbox a) =>
  Hld.Hld ->
  Commutativity ->
  VU.Vector a ->
  ST s (TreeMonoid a s)
fromVertsST hld@Hld.Hld {indexHld} commuteTM xs_ = do
  let !_ = ACIA.runtimeAssert (VU.length indexHld == VU.length xs_) $ "AtCoder.Extra.Tree.TreeMonoid.fromVertsST: vertex number mismatch (`" ++ show (VU.length indexHld) ++ "` and `" ++ show (VU.length xs_) ++ "`)"
  let !xs = VU.create $ do
        vec <- VUM.unsafeNew $ VU.length xs_
        VU.iforM_ xs_ $ \i x -> do
          VGM.write vec (indexHld VG.! i) x
        pure vec
  buildST hld commuteTM Hld.WeightsAreOnVertices xs

{-# INLINEABLE fromEdgesST #-}
fromEdgesST ::
  (HasCallStack, Monoid a, VU.Unbox a) =>
  Hld.Hld ->
  Commutativity ->
  VU.Vector (Vertex, Vertex, a) ->
  ST s (TreeMonoid a s)
fromEdgesST hld@Hld.Hld {indexHld} commuteTM edges = do
  let !xs = VU.create $ do
        vec <- VUM.unsafeNew $ VU.length indexHld
        VU.forM_ edges $ \(!u, !v, !w) -> do
          let u' = indexHld VG.! u
          let v' = indexHld VG.! v
          VGM.write vec (max u' v') w
        pure vec
  buildST hld commuteTM Hld.WeightsAreOnEdges xs

{-# INLINEABLE prodST #-}
prodST :: (HasCallStack, Monoid a, VU.Unbox a) => TreeMonoid a s -> Vertex -> Vertex -> ST s a
prodST TreeMonoid {..} u v = do
  case commuteTM of
    Commute -> Hld.prod weightPolicyTM hldTM (ST.prod segFTM) (ST.prod segFTM) u v
    NonCommute -> Hld.prod weightPolicyTM hldTM (ST.prod segFTM) (\l r -> getDual <$> ST.prod segBTM l r) u v

{-# INLINEABLE prodSubtreeST #-}
prodSubtreeST :: (HasCallStack, Monoid a, VU.Unbox a) => TreeMonoid a s -> Vertex -> ST s a
prodSubtreeST TreeMonoid {..} subtreeRoot = do
  let (!l, !r) = Hld.subtreeSegmentInclusive hldTM subtreeRoot
  case weightPolicyTM of
    Hld.WeightsAreOnVertices -> ST.prod segFTM l (r + 1)
    Hld.WeightsAreOnEdges -> do
      -- ignore the root of the subtree, which has the minimum index among the subtree vertices
      if l == r
        then pure mempty
        else ST.prod segFTM (l + 1) (r + 1)

{-# INLINEABLE readST #-}
readST :: (HasCallStack, Monoid a, VU.Unbox a) => TreeMonoid a s -> Vertex -> ST s a
readST TreeMonoid {..} i_ = do
  let !i = Hld.indexHld hldTM VG.! i_
  ST.read segFTM i

{-# INLINEABLE writeST #-}
writeST :: (HasCallStack, Monoid a, VU.Unbox a) => TreeMonoid a s -> Vertex -> a -> ST s ()
writeST TreeMonoid {..} i_ x = do
  let !i = Hld.indexHld hldTM VG.! i_
  ST.write segFTM i x
  when (commuteTM == NonCommute) $ do
    ST.write segBTM i $ Dual x

{-# INLINEABLE exchangeST #-}
exchangeST :: (HasCallStack, Monoid a, VU.Unbox a) => TreeMonoid a s -> Vertex -> a -> ST s a
exchangeST TreeMonoid {..} i_ x = do
  let !i = Hld.indexHld hldTM VG.! i_
  !res <- ST.exchange segFTM i x
  when (commuteTM == NonCommute) $ do
    ST.write segBTM i $ Dual x
  pure res

{-# INLINEABLE modifyST #-}
modifyST :: (HasCallStack, Monoid a, VU.Unbox a) => TreeMonoid a s -> (a -> a) -> Int -> ST s ()
modifyST TreeMonoid {..} f i_ = do
  let !i = Hld.indexHld hldTM VG.! i_
  ST.modify segFTM f i
  when (commuteTM == NonCommute) $ do
    ST.modify segBTM (Dual . f . getDual) i
