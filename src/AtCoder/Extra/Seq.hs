{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- | Dynamic sequance of monoid values and actions.
--
-- ==== __Example__
--
-- Create a `Seq` of length \(10\):
--
-- >>> import AtCoder.Extra.Monoid.RangeAdd qualified as RangeAdd
-- >>> import AtCoder.Extra.Seq qualified as Seq
-- >>> import AtCoder.LazySegTree (SegAct (..))
-- >>> import Data.Semigroup (Sum (..))
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> seq <- Seq.new @_ @(RangeAdd.RangeAdd (Sum Int)) @(Sum Int) 10
--
-- Create a sequence of \(0, 1, 2, 3\):
--
-- >>> root <- Seq.newSeq seq (VU.fromList [0, 1, 2, 3])
--
-- Get monoid products:
--
-- >>> Seq.prodAll seq root
-- Sum {getSum = 6}
--
-- >>> Seq.prod seq root 1 3
-- Sum {getSum = 3}
--
-- `read`, `write`, `modify` and `exchange` are available:
--
-- >>> -- [0, 1, 2, 3] -> [0, 10, 2, 30]
-- >>> Seq.write seq root 3 30
-- >>> Seq.modify seq root (* 10) 1
--
-- Actions can be performed with @SegAct@ instances:
--
-- >>> -- [0, 10, 2, 30] -> [0, 20, 12, 40]
-- >>> Seq.applyIn seq root 1 4 (RangeAdd.new 10)
-- >>> Seq.prod seq root 1 4
-- Sum {getSum = 72}
--
-- The sequence can be reversed if the action type is commutative:
--
-- >>> Seq.reverse seq root 0 4
-- >>> VU.map getSum <$> Seq.freeze seq root
-- [40,12,20,0]
--
-- `Seq` is dynamic and new elements can be inserted or deleted:
--
-- >>> -- [40,33,12,20,0]
-- >>> Seq.insert seq root 1 (Sum 33)
-- >>> -- [40,33,12,0]
-- >>> Seq.delete seq root 3
-- >>> VU.map getSum <$> Seq.freeze seq root
-- [40,33,12,0]
--
-- The `Seq` can store multiple sequences:
--
-- >>> root2 <- Seq.newSeq seq (VU.generate 2 Sum)
-- >>> VU.map getSum <$> Seq.freeze seq root2
-- [0,1]
--
-- Merge/split operations are available. They mutate the given @root@ to be the leftmost one:
--
-- >>> Seq.merge seq root root2
-- >>> VU.map getSum <$> Seq.freeze seq root
-- [40,33,12,0,0,1]
--
-- >>> (rootM, rootR) <- Seq.split3 seq root 2 4
-- >>> VU.map getSum <$> Seq.freeze seq root
-- [40,33]
--
-- >>> VU.map getSum <$> Seq.freeze seq rootM
-- [12,0]
--
-- >>> VU.map getSum <$> Seq.freeze seq rootR
-- [0,1]
--
-- Bisection methods are available:
--
-- >>> Seq.reset seq
-- >>> root <- Seq.newSeq seq $ VU.generate 10 Sum
-- >>> Seq.lowerBound seq root (\_ x -> x < 5)
-- 5
--
-- >>> Seq.lowerBoundProd seq root (\_ x -> x < 5)
-- 2
--
-- @since 1.2.0.0
module AtCoder.Extra.Seq
  ( -- * Seq
    Seq.Seq (..),
    Handle (..),

    -- * Constructors
    new,
    newHandle,
    reset,

    -- ** Nodes
    freeNode,
    freeSubtree,
    newNode,
    newSeq,

    -- * Merge/split
    merge,
    merge3,
    merge4,
    split,
    split3,
    split4,
    splitLr,
    -- slice, -- because it returns a raw `P.Index`, use the `Raw.sliceST` instead

    -- * Read/write
    read,
    write,
    modify,
    exchange,

    -- * Products
    prod,
    prodMaybe,
    prodAll,

    -- * Applications
    applyIn,
    applyToRoot,
    reverse,

    -- * Insert/delete
    insert,
    delete,
    detach,

    -- * Balancing
    splay,
    splayKth,

    -- * Bisection methods

    -- ** C++-like bisection methods
    lowerBound,
    lowerBoundM,
    lowerBoundProd,
    lowerBoundProdM,

    -- ** Splits
    splitMaxRight,
    splitMaxRightM,
    splitMaxRightProd,
    splitMaxRightProdM,

    -- ** Splits

    -- Max right functions are not provided, because they don't update root
    -- maxRight,
    -- maxRightM,
    -- maxRightProd,
    -- maxRightProdM,

    -- * Conversions
    freeze,
  )
where

import AtCoder.Extra.Pool qualified as P
import AtCoder.Extra.Seq.Raw (Seq (..))
import AtCoder.Extra.Seq.Raw qualified as Seq
import AtCoder.LazySegTree (SegAct (..))
import Control.Monad.Primitive (PrimMonad, PrimState, stToPrim)
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Stack (HasCallStack)
import Prelude hiding (read, reverse, seq)

-- | Handle of a sequence in `Seq`. Internally it stores the root vertex and tracks wits change.
--
-- @since 1.2.0.0
newtype Handle s = Handle
  { -- | @since 1.2.0.0
    unHandle :: VUM.MVector s P.Index
  }

-- | \(O(n)\) Creates a new `Seq` of length \(n\).
--
-- @since 1.2.0.0
{-# INLINE new #-}
new :: (PrimMonad m, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Int -> m (Seq (PrimState m) f a)
new n = stToPrim $ Seq.newST n

-- | \(O(1)\) Creates a new sequence `Handle` from a root vertex index.
{-# INLINE newHandle #-}
newHandle :: (PrimMonad m) => P.Index -> m (Handle (PrimState m))
newHandle x = stToPrim $ Handle <$> VUM.replicate 1 x

-- | \(O(1)\) Clear all the elements in the sequence.
--
-- @since 1.2.0.0
{-# INLINE reset #-}
reset :: (PrimMonad m) => Seq (PrimState m) f a -> m ()
reset seq = stToPrim $ Seq.resetST seq

-- | \(O(1)\) Allocates a node.
--
-- @since 1.2.0.0
{-# INLINE newNode #-}
newNode :: (PrimMonad m, Monoid f, VU.Unbox f, VU.Unbox a) => Seq (PrimState m) f a -> a -> m (Handle (PrimState m))
newNode seq x = stToPrim $ newHandle =<< Seq.newNodeST seq x

-- | \(O(n)\) Allocates a sequence.
--
-- @since 1.2.0.0
{-# INLINE newSeq #-}
newSeq :: (PrimMonad m, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq (PrimState m) f a -> VU.Vector a -> m (Handle (PrimState m))
newSeq seq !xs = stToPrim $ newHandle =<< Seq.newSeqST seq xs

-- | \(O(1)\) Frees a node.
--
-- @since 1.2.0.0
{-# INLINE freeNode #-}
freeNode :: (PrimMonad m) => Seq (PrimState m) v a -> Handle (PrimState m) -> m ()
freeNode seq (Handle handle) = do
  VGM.unsafeModifyM
    handle
    ( \i -> do
        stToPrim $ Seq.freeNodeST seq i
        pure P.undefIndex
    )
    0

-- | \(O(n)\) Frees a subtree.
--
-- @since 1.2.0.0
{-# INLINE freeSubtree #-}
freeSubtree :: (PrimMonad m, VU.Unbox a) => Seq (PrimState m) f a -> Handle (PrimState m) -> m ()
freeSubtree seq (Handle handle) = stToPrim $ do
  c0 <- VGM.unsafeRead handle 0
  Seq.freeSubtreeST seq c0

-- -------------------------------------------------------------------------------------------------
-- Merge/split
-- -------------------------------------------------------------------------------------------------

-- | Amortized \(O(\log n)\). Merges two sequences into one. Handles other than the leftmost will be
-- invalidates.
--
-- ==== Constraints
-- - The vertices must be roots.
--
-- @since 1.2.0.0
{-# INLINE merge #-}
merge :: (PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq (PrimState m) f a -> Handle (PrimState m) -> Handle (PrimState m) -> m ()
merge seq (Handle l) (Handle r) = stToPrim $ do
  lRoot <- VGM.unsafeRead l 0
  rRoot <- VGM.unsafeRead r 0
  root' <- Seq.mergeST seq lRoot rRoot
  VGM.unsafeWrite l 0 root'
  VGM.unsafeWrite r 0 P.undefIndex

-- | Amortized \(O(\log n)\). Merges three sequences into one. Handles other than the leftmost will
-- be invalidates.
--
-- ==== Constraints
-- - The vertices must be roots.
--
-- @since 1.2.0.0
{-# INLINE merge3 #-}
merge3 :: (PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq (PrimState m) f a -> Handle (PrimState m) -> Handle (PrimState m) -> Handle (PrimState m) -> m ()
merge3 seq (Handle hA) (Handle hB) (Handle hC) = stToPrim $ do
  a <- VGM.unsafeRead hA 0
  b <- VGM.unsafeRead hB 0
  c <- VGM.unsafeRead hC 0
  root' <- Seq.merge3ST seq a b c
  VGM.unsafeWrite hA 0 root'
  VGM.unsafeWrite hB 0 P.undefIndex
  VGM.unsafeWrite hC 0 P.undefIndex

-- | Amortized \(O(\log n)\). Merges four sequences into one. Handles other than the leftmost will
-- be invalidates.
--
-- ==== Constraints
-- - The vertices must be roots.
--
-- @since 1.2.0.0
{-# INLINE merge4 #-}
merge4 :: (PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq (PrimState m) f a -> Handle (PrimState m) -> Handle (PrimState m) -> Handle (PrimState m) -> Handle (PrimState m) -> m ()
merge4 seq (Handle hA) (Handle hB) (Handle hC) (Handle hD) = stToPrim $ do
  a <- VGM.unsafeRead hA 0
  b <- VGM.unsafeRead hB 0
  c <- VGM.unsafeRead hC 0
  d <- VGM.unsafeRead hC 0
  root' <- Seq.merge4ST seq a b c d
  VGM.unsafeWrite hA 0 root'
  VGM.unsafeWrite hB 0 P.undefIndex
  VGM.unsafeWrite hC 0 P.undefIndex
  VGM.unsafeWrite hD 0 P.undefIndex

-- | Amortized \(O(\log n)\). Splits a sequences into two: \([0, k), [k, n)\).
--
-- @since 1.2.0.0
{-# INLINE split #-}
split :: (PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq (PrimState m) f a -> Handle (PrimState m) -> Int -> m (Handle (PrimState m))
split seq (Handle hRoot) k = stToPrim $ do
  root <- VGM.unsafeRead hRoot 0
  (!r1, !r2) <- Seq.splitST seq root k
  VGM.unsafeWrite hRoot 0 r1
  newHandle r2

-- | Amortized \(O(\log n)\). Splits a sequences into three: \([0, l), [l, r), [r, n)\). Returns the non-leftmost sequences.
--
-- @since 1.2.0.0
{-# INLINE split3 #-}
split3 :: (PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq (PrimState m) f a -> Handle (PrimState m) -> Int -> Int -> m (Handle (PrimState m), Handle (PrimState m))
split3 seq (Handle hRoot) l r = stToPrim $ do
  root <- VGM.unsafeRead hRoot 0
  (!r1, !r2, !r3) <- Seq.split3ST seq root l r
  VGM.unsafeWrite hRoot 0 r1
  (,) <$> newHandle r2 <*> newHandle r3

-- | Amortized \(O(\log n)\). Splits a sequences into four: \([0, i), [i, j), [j, k), [k, n)\). Returns the non-leftmost sequences.
--
-- @since 1.2.0.0
{-# INLINE split4 #-}
split4 :: (PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq (PrimState m) f a -> Handle (PrimState m) -> Int -> Int -> Int -> m (Handle (PrimState m), Handle (PrimState m), Handle (PrimState m))
split4 seq (Handle hRoot) i j k = stToPrim $ do
  root <- VGM.unsafeRead hRoot 0
  (!r1, !r2, !r3, !r4) <- Seq.split4ST seq root i j k
  VGM.unsafeWrite hRoot 0 r1
  (,,) <$> newHandle r2 <*> newHandle r3 <*> newHandle r4

-- | Amortized \(O(\log n)\). Splits a sequence into three: \([0, \mathrm{root}), \mathrm{root}, [\mathrm{root} + 1, n)\).
--
-- @since 1.2.0.0
{-# INLINE splitLr #-}
splitLr :: (PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq (PrimState m) f a -> Handle (PrimState m) -> m (Handle (PrimState m), Handle (PrimState m))
splitLr seq (Handle hRoot) = stToPrim $ do
  root <- VGM.unsafeRead hRoot 0
  (!l, !root', !r) <- Seq.splitLrST seq root
  VGM.unsafeWrite hRoot 0 root'
  (,) <$> newHandle l <*> newHandle r

-- -------------------------------------------------------------------------------------------------
-- Modifications
-- -------------------------------------------------------------------------------------------------

-- | Amortized \(O(\log n)\). Reads the \(k\)-th node's monoid value.
--
-- @since 1.2.0.0
{-# INLINE read #-}
read :: (PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq (PrimState m) f a -> Handle (PrimState m) -> Int -> m a
read seq (Handle hRoot) k = stToPrim $ do
  root <- VGM.unsafeRead hRoot 0
  (!v, !root') <- Seq.readST seq root k
  VGM.unsafeWrite hRoot 0 root'
  pure v

-- | Amortized \(O(\log n)\). Writes the \(k\)-th node's monoid value.
--
-- @since 1.2.0.0
{-# INLINE write #-}
write :: (PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq (PrimState m) f a -> Handle (PrimState m) -> Int -> a -> m ()
write seq (Handle hRoot) k v = stToPrim $ do
  VGM.unsafeModifyM
    hRoot
    ( \root -> do
        Seq.writeST seq root k v
    )
    0

-- | Amortized \(O(\log n)\). Modifies the \(k\)-th node's monoid value.
--
-- @since 1.2.0.0
{-# INLINE modify #-}
modify :: (PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq (PrimState m) f a -> Handle (PrimState m) -> (a -> a) -> Int -> m ()
modify seq (Handle hRoot) f k = stToPrim $ do
  VGM.unsafeModifyM
    hRoot
    ( \root -> do
        Seq.modifyST seq root f k
    )
    0

-- | Amortized \(O(\log n)\). Exchanges the \(k\)-th node's monoid value.
--
-- @since 1.2.0.0
{-# INLINE exchange #-}
exchange :: (PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq (PrimState m) f a -> Handle (PrimState m) -> Int -> a -> m a
exchange seq (Handle hRoot) k v = stToPrim $ do
  root <- VGM.unsafeRead hRoot 0
  (!x, !root') <- Seq.exchangeST seq root k v
  VGM.unsafeWrite hRoot 0 root'
  pure x

-- | Amortized \(O(\log n)\). Returns the monoid product in an interval \([l, r)\).
--
-- ==== Constraints
-- - \(0 \le l \le r \le n\)
--
-- @since 1.2.0.0
{-# INLINE prod #-}
prod :: (HasCallStack, PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq (PrimState m) f a -> Handle (PrimState m) -> Int -> Int -> m a
prod seq (Handle hRoot) l r = stToPrim $ do
  root <- VGM.unsafeRead hRoot 0
  (!v, !root') <- Seq.prodST seq root l r
  VGM.unsafeWrite hRoot 0 root'
  pure v

-- | Amortized \(O(\log n)\). Returns the monoid product in an interval \([l, r)\). Returns
-- `Nothing` if an invalid interval is given.
--
-- @since 1.2.0.0
{-# INLINE prodMaybe #-}
prodMaybe :: (HasCallStack, PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq (PrimState m) f a -> Handle (PrimState m) -> Int -> Int -> m (Maybe a)
prodMaybe seq (Handle handle) l r = stToPrim $ do
  root <- VGM.unsafeRead handle 0
  res <- Seq.prodMaybeST seq root l r
  case res of
    Just (!v, !root') -> do
      VGM.unsafeWrite handle 0 root'
      pure $ Just v
    Nothing -> pure Nothing

-- | Amortized \(O(\log n)\). Returns the monoid product of the whole sequence.
--
-- @since 1.2.0.0
{-# INLINE prodAll #-}
prodAll :: (PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq (PrimState m) f a -> Handle (PrimState m) -> m a
prodAll seq (Handle handle) = stToPrim $ do
  root <- VGM.unsafeRead handle 0
  Seq.prodAllST seq root

-- | \(O(1)\) Given a root vertex \(r\), applies an action \(f\) to it. It does nothing if \(r\)
-- is not a root.
--
-- @since 1.2.0.0
{-# INLINE applyToRoot #-}
applyToRoot :: (PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq (PrimState m) f a -> Handle (PrimState m) -> f -> m ()
applyToRoot seq (Handle hRoot) act = stToPrim $ do
  root <- VGM.unsafeRead hRoot 0
  Seq.applyToRootST seq root act

-- | Amortized \(O(\log n)\). Applies an action \(f\) to \([l, r)\).
--
-- ==== Constraints
-- - \(0 \leq l \leq r \leq n\)
--
-- @since 1.2.0.0
{-# INLINE applyIn #-}
applyIn :: (HasCallStack, PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq (PrimState m) f a -> Handle (PrimState m) -> Int -> Int -> f -> m ()
applyIn seq (Handle hRoot) l r act = stToPrim $ do
  VUM.unsafeModifyM
    hRoot
    ( \root -> do
        Seq.applyInST seq root l r act
    )
    0

-- | Amortized \(O(\log n)\). Reverses the sequence in \([l, r)\).
--
-- ==== Constraints
-- - The monoid action \(f\) must be commutative.
-- - The monoid value \(v\) must be commutative.
--
-- @since 1.2.0.0
{-# INLINE reverse #-}
reverse :: (PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq (PrimState m) f a -> Handle (PrimState m) -> Int -> Int -> m ()
reverse seq (Handle hRoot) l r = stToPrim $ do
  VUM.unsafeModifyM
    hRoot
    ( \root -> do
        Seq.reverseST seq root l r
    )
    0

-- | Amortized \(O(\log n)\). Inserts a new node at \(k\) with initial monoid value \(v\).
--
-- @since 1.2.0.0
{-# INLINE insert #-}
insert :: (PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq (PrimState m) f a -> Handle (PrimState m) -> Int -> a -> m ()
insert seq (Handle hRoot) k v = stToPrim $ do
  VUM.unsafeModifyM
    hRoot
    ( \root -> do
        Seq.insertST seq root k v
    )
    0

-- | Amortized \(O(\log n)\). Frees a node at \(k\).
--
-- @since 1.2.0.0
{-# INLINE delete #-}
delete :: (PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq (PrimState m) f a -> Handle (PrimState m) -> Int -> m ()
delete seq (Handle hRoot) i = stToPrim $ do
  VUM.unsafeModifyM
    hRoot
    ( \root -> do
        Seq.deleteST seq root i
    )
    0

-- | Amortized \(O(\log n)\). Detaches a node at \(k\) from their connected components. Returns the
-- detached vertex `Handle`.
--
-- @since 1.2.0.0
{-# INLINE detach #-}
detach :: (PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq (PrimState m) f a -> Handle (PrimState m) -> Int -> m (Handle (PrimState m))
detach seq (Handle hRoot) i = stToPrim $ do
  root <- VUM.unsafeRead hRoot 0
  newHandle =<< Seq.detachST seq root i

-- -------------------------------------------------------------------------------------------------
-- Balancing
-- -------------------------------------------------------------------------------------------------

-- | Amortized \(O(\log n)\). Moves up a vertex to be a root.
--
-- @since 1.2.0.0
{-# INLINE splay #-}
splay :: (PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq (PrimState m) f a -> Handle (PrimState m) -> Bool -> m ()
splay seq (Handle hRoot) doneParentProp = stToPrim $ do
  i <- VGM.unsafeRead hRoot 0
  Seq.splayST seq i doneParentProp

-- | Amortized \(O(\log n)\). Finds \(k\)-th node and splays it.
-- Returns the new root.
--
-- @since 1.2.0.0
{-# INLINE splayKth #-}
splayKth :: (PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq (PrimState m) f a -> Handle (PrimState m) -> Int -> m ()
splayKth seq (Handle hRoot) k0 = stToPrim $ do
  VGM.unsafeModifyM
    hRoot
    ( \root -> do
        Seq.splayKthST seq root k0
    )
    0

-- -------------------------------------------------------------------------------------------------
-- Bisection methods
-- -------------------------------------------------------------------------------------------------

{-# INLINE lowerBound #-}
lowerBound ::
  (PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) =>
  -- | Sequence of monoid values
  Seq (PrimState m) f a ->
  -- | Root handle
  Handle (PrimState m) ->
  -- | User predicate that takes the index and the monoid value.
  (Int -> a -> Bool) ->
  -- |
  m Int
lowerBound seq (Handle root0) f = stToPrim $ do
  root <- VUM.unsafeRead root0 0
  (!r, !root') <- Seq.lowerBoundST seq root f
  VUM.unsafeWrite root0 0 root'
  pure r

{-# INLINE lowerBoundM #-}
lowerBoundM ::
  (PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) =>
  -- | Sequence of monoid values
  Seq (PrimState m) f a ->
  -- | Root handle
  Handle (PrimState m) ->
  -- | User predicate that takes the index and the monoid value.
  (Int -> a -> m Bool) ->
  -- |
  m Int
lowerBoundM seq (Handle root0) f = do
  root <- VUM.unsafeRead root0 0
  (!r, !root') <- Seq.lowerBoundM seq root f
  VUM.unsafeWrite root0 0 root'
  pure r

{-# INLINE lowerBoundProd #-}
lowerBoundProd ::
  (PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) =>
  -- | Sequence of monoid values
  Seq (PrimState m) f a ->
  -- | Root handle
  Handle (PrimState m) ->
  -- | User predicate that takes the index and the monoid values.
  (Int -> a -> Bool) ->
  -- |
  m Int
lowerBoundProd seq (Handle root0) f = stToPrim $ do
  root <- VUM.unsafeRead root0 0
  (!r, !root') <- Seq.lowerBoundProdST seq root f
  VUM.unsafeWrite root0 0 root'
  pure r

{-# INLINE lowerBoundProdM #-}
lowerBoundProdM ::
  (PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) =>
  -- | Sequence of monoid values
  Seq (PrimState m) f a ->
  -- | Root handle
  Handle (PrimState m) ->
  -- | User predicate that takes the index and the monoid values.
  (Int -> a -> m Bool) ->
  -- |
  m Int
lowerBoundProdM seq (Handle root0) f = do
  root <- VUM.unsafeRead root0 0
  (!r, !root') <- Seq.lowerBoundProdM seq root f
  VUM.unsafeWrite root0 0 root'
  pure r

-- | Amortized \(O(\log n)\). Given a monotonious sequence, returns the rightmost vertex \(v_k\)
-- where \(f(v)\) holds for every \([0, i) (0 \le i \lt k)\).
{-# INLINE splitMaxRight #-}
splitMaxRight ::
  (PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) =>
  -- | Sequence of monoid values
  Seq (PrimState m) f a ->
  -- | Root vertex
  Handle (PrimState m) ->
  -- | User predicate that takes the *length* \(n = k + 1\) and the monoid product of \([0, k)\).
  (Int -> a -> Bool) ->
  -- | Right sequences where \(f\) holds for the left
  m (Handle (PrimState m))
splitMaxRight seq (Handle root0) f = stToPrim $ do
  root <- VGM.unsafeRead root0 0
  (!l, !r) <- Seq.splitMaxRightST seq root f
  VGM.unsafeWrite root0 0 l
  newHandle r

-- | Amortized \(O(\log n)\). Given a monotonious sequence, returns the rightmost vertex \(v_k\)
-- where \(f(v)\) holds for every \([0, i) (0 \le i \lt k)\).
{-# INLINEABLE splitMaxRightM #-}
splitMaxRightM ::
  (PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) =>
  -- | Sequence of monoid values
  Seq (PrimState m) f a ->
  -- | Root vertex
  Handle (PrimState m) ->
  -- | User predicate that takes the *length* \(n = k + 1\) and the monoid product of \([0, k)\).
  (Int -> a -> m Bool) ->
  -- | (left, right) sequences where \(f\) holds for the left
  m (Handle (PrimState m))
splitMaxRightM seq (Handle root0) f = do
  root <- VGM.unsafeRead root0 0
  (!l, !r) <- Seq.splitMaxRightM seq root f
  VGM.unsafeWrite root0 0 l
  newHandle r

-- | Amortized \(O(\log n)\). Given a monotonious sequence, returns the rightmost vertex \(v_k\)
-- where \(f(v)\) holds for every \([0, i) (0 \le i \lt k)\).
{-# INLINE splitMaxRightProd #-}
splitMaxRightProd ::
  (PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) =>
  -- | Sequence of monoid values
  Seq (PrimState m) f a ->
  -- | Root vertex
  Handle (PrimState m) ->
  -- | User predicate that takes the *length* \(n = k + 1\) and the monoid product of \([0, k)\).
  (Int -> a -> Bool) ->
  -- | (left, right) sequences where \(f\) holds for the left
  m (Handle (PrimState m))
splitMaxRightProd seq (Handle root0) f = stToPrim $ do
  root <- VGM.unsafeRead root0 0
  (!l, !r) <- Seq.splitMaxRightProdST seq root f
  VGM.unsafeWrite root0 0 l
  newHandle r

-- | Amortized \(O(\log n)\). Given a monotonious sequence, returns the rightmost vertex \(v_k\)
-- where \(f(v)\) holds for every \([0, i) (0 \le i \lt k)\).
{-# INLINEABLE splitMaxRightProdM #-}
splitMaxRightProdM ::
  (PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) =>
  -- | Sequence of monoid values
  Seq (PrimState m) f a ->
  -- | Root vertex
  Handle (PrimState m) ->
  -- | User predicate that takes the *length* \(n = k + 1\) and the monoid product of \([0, k)\).
  (Int -> a -> m Bool) ->
  -- | Right sequences where \(f\) holds for the left
  m (Handle (PrimState m))
splitMaxRightProdM seq (Handle root0) f = do
  root <- VGM.unsafeRead root0 0
  (!l, !r) <- Seq.splitMaxRightProdM seq root f
  VGM.unsafeWrite root0 0 l
  newHandle r

-- -------------------------------------------------------------------------------------------------
-- Conversions
-- -------------------------------------------------------------------------------------------------

-- | Amortized \(O(n \log n)\). Returns the sequence of monoid values.
--
-- @since 1.2.0.0
{-# INLINE freeze #-}
freeze :: (HasCallStack, PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq (PrimState m) f a -> Handle (PrimState m) -> m (VU.Vector a)
freeze seq (Handle hRoot) = stToPrim $ do
  root <- VGM.unsafeRead hRoot 0
  Seq.freezeST seq root
