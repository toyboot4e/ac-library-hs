{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}

-- | Dynamic sequence of monoid values with monoid actions on them through the `SegAct` instance.
--
-- ==== Performance
-- This module is __slow__ as an ordinary dynamic sequence. Consider using another module if you
-- don't need monoid products.
--
-- ==== __Example__
--
-- Create a `Seq` storage of length \(10\):
--
-- >>> import AtCoder.Extra.Monoid.RangeAdd qualified as RangeAdd
-- >>> import AtCoder.Extra.Seq qualified as Seq
-- >>> import AtCoder.LazySegTree (SegAct (..))
-- >>> import Data.Semigroup (Sum (..))
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> seq <- Seq.new @_ @(RangeAdd.RangeAdd (Sum Int)) @(Sum Int) 10
--
-- Allocate a sequence of \(0, 1, 2, 3\):
--
-- >>> handle <- Seq.newSeq seq (VU.fromList [0, 1, 2, 3])
--
-- Get monoid products:
--
-- >>> Seq.prodAll seq handle
-- Sum {getSum = 6}
--
-- >>> Seq.prod seq handle 1 3
-- Sum {getSum = 3}
--
-- `read`, `write`, `modify` and `exchange` are available:
--
-- >>> -- [0, 1, 2, 3] -> [0, 10, 2, 30]
-- >>> Seq.write seq handle 3 30
-- >>> Seq.modify seq handle (* 10) 1
--
-- Actions can be performed with `SegAct` instances:
--
-- >>> -- [0, 10, 2, 30] -> [0, 20, 12, 40]
-- >>> Seq.applyIn seq handle 1 4 (RangeAdd.new 10)
-- >>> Seq.prod seq handle 1 4
-- Sum {getSum = 72}
--
-- The sequence can be reversed if the action type is commutative:
--
-- >>> Seq.reverse seq handle 0 4
-- >>> VU.map getSum <$> Seq.freeze seq handle
-- [40,12,20,0]
--
-- The sequence is dynamic and new elements can be inserted or deleted:
--
-- >>> -- [40, 12, 20, 0] -> [40, 33, 12, 20, 0]
-- >>> Seq.insert seq handle 1 (Sum 33)
-- >>> -- [40, 33, 12, 20, 0] -> [40, 33, 12, 0]
-- >>> Seq.delete seq handle 3
-- Sum {getSum = 20}
--
-- >>> VU.map getSum <$> Seq.freeze seq handle
-- [40,33,12,0]
--
-- The `Seq` storage can store multiple sequences:
--
-- >>> handle2 <- Seq.newSeq seq (VU.generate 2 Sum)
-- >>> VU.map getSum <$> Seq.freeze seq handle2
-- [0,1]
--
-- Merge/split operations are available. `merge` functions mutate the given @handle@ to be the
-- merged sequence handle:
--
-- >>> Seq.merge seq handle handle2
-- >>> VU.map getSum <$> Seq.freeze seq handle
-- [40,33,12,0,0,1]
--
-- `split` functions mutate the given @handle@ to be the leftmost one and returns right sequence
-- handles:
--
-- >>> (handleM, handleR) <- Seq.split3 seq handle 2 4
-- >>> VU.map getSum <$> Seq.freeze seq handle
-- [40,33]
--
-- >>> VU.map getSum <$> Seq.freeze seq handleM
-- [12,0]
--
-- >>> VU.map getSum <$> Seq.freeze seq handleR
-- [0,1]
--
-- Bisection methods are available for monoid values and their products:
--
-- >>> Seq.reset seq
-- >>> handle <- Seq.newSeq seq $ VU.generate 10 Sum
-- >>> Seq.ilowerBound seq handle (\_ x -> x < 5)
-- 5
--
-- >>> Seq.ilowerBoundProd seq handle (\_ x -> x < 5)
-- 3
--
-- @since 1.2.0.0
module AtCoder.Extra.Seq
  ( -- * Seq
    Seq.Seq (..),
    Handle (..),
    newHandle,
    nullHandle,
    invalidateHandle,

    -- * Constructors
    new,
    reset,
    free,
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
    readMaybe,
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
    delete_,
    detach,

    -- * Bisection methods

    -- ** C++-like
    ilowerBound,
    ilowerBoundM,
    ilowerBoundProd,
    ilowerBoundProdM,

    -- ** Splits
    isplitMaxRight,
    isplitMaxRightM,
    isplitMaxRightProd,
    isplitMaxRightProdM,

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

-- | `Handle` for a sequence in `Seq`. It internally stores the root node and updates it
-- following splaying operations, as `Seq` utilizes a splay tree structure.
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

-- | \(O(1)\) Creates a new sequence `Handle` from a root node index.
--
-- @since 1.2.0.0
{-# INLINE newHandle #-}
newHandle :: (PrimMonad m) => P.Index -> m (Handle (PrimState m))
newHandle x = stToPrim $ Handle <$> VUM.replicate 1 x

-- | \(O(1)\) Returns whether the sequence is empty.
--
-- @since 1.2.0.0
{-# INLINE nullHandle #-}
nullHandle :: (PrimMonad m) => Handle (PrimState m) -> m Bool
nullHandle (Handle h) = stToPrim $ do
  P.nullIndex <$> VGM.unsafeRead h 0

-- | \(O(1)\) Invalidates a sequence handle. Note that it does not change or `free` the sequence.
--
-- @since 1.2.0.0
{-# INLINE invalidateHandle #-}
invalidateHandle :: (PrimMonad m) => Handle (PrimState m) -> m ()
invalidateHandle (Handle h) = stToPrim $ do
  VGM.unsafeWrite h 0 P.undefIndex

-- | \(O(1)\) Clears the sequence storage. All the handles must not be used again.
--
-- @since 1.2.0.0
{-# INLINE reset #-}
reset :: (PrimMonad m) => Seq (PrimState m) f a -> m ()
reset seq = stToPrim $ Seq.resetST seq

-- | \(O(1)\) Allocates a new sequence of length \(1\).
--
-- @since 1.2.0.0
{-# INLINE newNode #-}
newNode :: (PrimMonad m, Monoid f, VU.Unbox f, VU.Unbox a) => Seq (PrimState m) f a -> a -> m (Handle (PrimState m))
newNode seq x = stToPrim $ newHandle =<< Seq.newNodeST seq x

-- | \(O(n)\) Allocates a new sequence.
--
-- @since 1.2.0.0
{-# INLINE newSeq #-}
newSeq :: (PrimMonad m, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq (PrimState m) f a -> VU.Vector a -> m (Handle (PrimState m))
newSeq seq !xs = stToPrim $ newHandle =<< Seq.newSeqST seq xs

-- | \(O(n)\) Frees a sequence and invalidates the handle.
--
-- @since 1.2.0.0
{-# INLINE free #-}
free :: (PrimMonad m, VU.Unbox a) => Seq (PrimState m) f a -> Handle (PrimState m) -> m ()
free seq (Handle handle) = stToPrim $ do
  c0 <- VGM.unsafeRead handle 0
  Seq.freeSubtreeST seq c0
  VGM.write handle 0 P.undefIndex

-- -------------------------------------------------------------------------------------------------
-- Merge/split
-- -------------------------------------------------------------------------------------------------

-- | Amortized \(O(\log n)\). Merges two sequences \(l, r\) into one in the given order, ignoring
-- empty sequences. The right sequence handle will be invalidated.
--
-- @since 1.2.0.0
{-# INLINE merge #-}
merge :: (HasCallStack, PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq (PrimState m) f a -> Handle (PrimState m) -> Handle (PrimState m) -> m ()
merge seq (Handle l) (Handle r) = stToPrim $ do
  lRoot <- VGM.unsafeRead l 0
  rRoot <- VGM.unsafeRead r 0
  root' <- Seq.mergeST seq lRoot rRoot
  VGM.unsafeWrite l 0 root'
  VGM.unsafeWrite r 0 P.undefIndex

-- | Amortized \(O(\log n)\). Merges three sequences \(l, m, r\) into one in the given order,
-- ignoring empty sequences. All handles, except for the leftmost one, will be invalidated.
--
-- @since 1.2.0.0
{-# INLINE merge3 #-}
merge3 :: (HasCallStack, PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq (PrimState m) f a -> Handle (PrimState m) -> Handle (PrimState m) -> Handle (PrimState m) -> m ()
merge3 seq (Handle hA) (Handle hB) (Handle hC) = stToPrim $ do
  a <- VGM.unsafeRead hA 0
  b <- VGM.unsafeRead hB 0
  c <- VGM.unsafeRead hC 0
  root' <- Seq.merge3ST seq a b c
  VGM.unsafeWrite hA 0 root'
  VGM.unsafeWrite hB 0 P.undefIndex
  VGM.unsafeWrite hC 0 P.undefIndex

-- | Amortized \(O(\log n)\). Merges four sequences \(a, b, c, d\) into one in the given order,
-- ignoring empty sequences. All handles, except for the leftmost one, will be invalidated.
--
-- ==== Constraints
-- - The vertices must be roots.
--
-- @since 1.2.0.0
{-# INLINE merge4 #-}
merge4 :: (HasCallStack, PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq (PrimState m) f a -> Handle (PrimState m) -> Handle (PrimState m) -> Handle (PrimState m) -> Handle (PrimState m) -> m ()
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

-- | Amortized \(O(\log n)\). Splits a sequences into two: \([0, k), [k, n)\). The handle will
-- point to the left sequence. Returns the right sequence handle.
--
-- ==== Constraints
-- - \(0 \le k \le n\).
--
-- @since 1.2.0.0
{-# INLINE split #-}
split :: (HasCallStack, PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq (PrimState m) f a -> Handle (PrimState m) -> Int -> m (Handle (PrimState m))
split seq (Handle hRoot) k = stToPrim $ do
  root <- VGM.unsafeRead hRoot 0
  (!r1, !r2) <- Seq.splitST seq root k
  VGM.unsafeWrite hRoot 0 r1
  newHandle r2

-- | Amortized \(O(\log n)\). Splits a sequences into three: \([0, l), [l, r), [r, n)\). The handle
-- will point to the leftmost sequence. Returns the middle and the right sequence handles.
--
-- ==== Constraints
-- - \(0 \le l \le r \le n\).
--
-- @since 1.2.0.0
{-# INLINE split3 #-}
split3 :: (HasCallStack, PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq (PrimState m) f a -> Handle (PrimState m) -> Int -> Int -> m (Handle (PrimState m), Handle (PrimState m))
split3 seq (Handle hRoot) l r = stToPrim $ do
  root <- VGM.unsafeRead hRoot 0
  (!r1, !r2, !r3) <- Seq.split3ST seq root l r
  VGM.unsafeWrite hRoot 0 r1
  (,) <$> newHandle r2 <*> newHandle r3

-- | Amortized \(O(\log n)\). Splits a sequences into four: \([0, i), [i, j), [j, k), [k, n)\).
-- The handle will point to the leftmost sequence. Returns the non-leftmost sequence handles.
--
-- ==== Constraints
-- - \(0 \le i \le j \le k \le n\).
--
-- @since 1.2.0.0
{-# INLINE split4 #-}
split4 :: (HasCallStack, PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq (PrimState m) f a -> Handle (PrimState m) -> Int -> Int -> Int -> m (Handle (PrimState m), Handle (PrimState m), Handle (PrimState m))
split4 seq (Handle hRoot) i j k = stToPrim $ do
  root <- VGM.unsafeRead hRoot 0
  (!r1, !r2, !r3, !r4) <- Seq.split4ST seq root i j k
  VGM.unsafeWrite hRoot 0 r1
  (,,) <$> newHandle r2 <*> newHandle r3 <*> newHandle r4

-- | Amortized \(O(\log n)\). Splits a sequence into three: \([0, \mathrm{root}), \mathrm{root}, [\mathrm{root} + 1, n)\).
--
-- ==== Constraints
-- - The sequence must be non-empty.
--
-- @since 1.2.0.0
{-# INLINE splitLr #-}
splitLr :: (HasCallStack, PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq (PrimState m) f a -> Handle (PrimState m) -> m (Handle (PrimState m), Handle (PrimState m))
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
-- ==== Constraints
-- - \(0 \le k \lt n\)
--
-- @since 1.2.0.0
{-# INLINE read #-}
read :: (HasCallStack, PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq (PrimState m) f a -> Handle (PrimState m) -> Int -> m a
read seq (Handle hRoot) k = stToPrim $ do
  root <- VGM.unsafeRead hRoot 0
  (!v, !root') <- Seq.readST seq root k
  VGM.unsafeWrite hRoot 0 root'
  pure v

-- | Amortized \(O(\log n)\). Reads the \(k\)-th node's monoid value.
--
-- @since 1.2.0.0
{-# INLINE readMaybe #-}
readMaybe :: (HasCallStack, PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq (PrimState m) f a -> Handle (PrimState m) -> Int -> m (Maybe a)
readMaybe seq (Handle hRoot) k = stToPrim $ do
  root <- VGM.unsafeRead hRoot 0
  res <- Seq.readMaybeST seq root k
  case res of
    Just (!v, !root') -> do
      VGM.unsafeWrite hRoot 0 root'
      pure $ Just v
    Nothing -> pure Nothing

-- | Amortized \(O(\log n)\). Writes to the \(k\)-th node's monoid value.
--
-- ==== Constraints
-- - \(0 \le k \lt n\)
--
-- @since 1.2.0.0
{-# INLINE write #-}
write :: (HasCallStack, PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq (PrimState m) f a -> Handle (PrimState m) -> Int -> a -> m ()
write seq (Handle hRoot) k v = stToPrim $ do
  VGM.unsafeModifyM
    hRoot
    ( \root -> do
        Seq.writeST seq root k v
    )
    0

-- | Amortized \(O(\log n)\). Modifies the \(k\)-th node's monoid value.
--
-- ==== Constraints
-- - \(0 \le k \lt n\)
--
-- @since 1.2.0.0
{-# INLINE modify #-}
modify :: (HasCallStack, PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq (PrimState m) f a -> Handle (PrimState m) -> (a -> a) -> Int -> m ()
modify seq (Handle hRoot) f k = stToPrim $ do
  VGM.unsafeModifyM
    hRoot
    ( \root -> do
        Seq.modifyST seq root f k
    )
    0

-- | Amortized \(O(\log n)\). Exchanges the \(k\)-th node's monoid value.
--
-- ==== Constraints
-- - \(0 \le k \lt n\)
--
-- @since 1.2.0.0
{-# INLINE exchange #-}
exchange :: (HasCallStack, PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq (PrimState m) f a -> Handle (PrimState m) -> Int -> a -> m a
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
prod :: (HasCallStack, Show a, PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq (PrimState m) f a -> Handle (PrimState m) -> Int -> Int -> m a
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
prodAll :: (HasCallStack, PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq (PrimState m) f a -> Handle (PrimState m) -> m a
prodAll seq (Handle handle) = stToPrim $ do
  root <- VGM.unsafeRead handle 0
  Seq.prodAllST seq root

-- | Amortized \(O(\log n)\). Given an interval \([l, r)\), applies a monoid action \(f\).
--
-- ==== Constraints
-- - \(0 \le l \le r \le n\)
--
-- @since 1.2.0.0
{-# INLINE applyIn #-}
applyIn :: (HasCallStack, PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq (PrimState m) f a -> Handle (PrimState m) -> Int -> Int -> f -> m ()
applyIn seq (Handle hRoot) l r act = stToPrim $ do
  VGM.unsafeModifyM
    hRoot
    ( \root -> do
        Seq.applyInST seq root l r act
    )
    0

-- | \(O(1)\) Applies a monoid action \(f\) to the root of a sequence.
--
-- @since 1.2.0.0
{-# INLINE applyToRoot #-}
applyToRoot :: (HasCallStack, PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq (PrimState m) f a -> Handle (PrimState m) -> f -> m ()
applyToRoot seq (Handle hRoot) act = stToPrim $ do
  root <- VGM.unsafeRead hRoot 0
  Seq.applyToRootST seq root act

-- | Amortized \(O(\log n)\). Reverses the sequence in \([l, r)\).
--
-- ==== Constraints
-- - The monoid action \(f\) must be commutative.
-- - The monoid value \(v\) must be commutative.
--
-- @since 1.2.0.0
{-# INLINE reverse #-}
reverse :: (HasCallStack, PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq (PrimState m) f a -> Handle (PrimState m) -> Int -> Int -> m ()
reverse seq (Handle hRoot) l r = stToPrim $ do
  VGM.unsafeModifyM
    hRoot
    ( \root -> do
        Seq.reverseST seq root l r
    )
    0

-- | Amortized \(O(\log n)\). Inserts a new node at \(k\) with initial monoid value \(v\). This
-- function works for an empty sequence handle.
--
-- ==== Constraints
-- - \(0 \le k \le n\)
--
-- @since 1.2.0.0
{-# INLINE insert #-}
insert :: (HasCallStack, PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq (PrimState m) f a -> Handle (PrimState m) -> Int -> a -> m ()
insert seq (Handle hRoot) k v = stToPrim $ do
  VGM.unsafeModifyM
    hRoot
    ( \root -> do
        Seq.insertST seq root k v
    )
    0

-- | Amortized \(O(\log n)\). Frees the \(k\)-th node and returns the monoid value of it.
--
-- ==== Constraints
-- - \(0 \le k \lt n\)
--
-- @since 1.2.0.0
{-# INLINE delete #-}
delete :: (HasCallStack, PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq (PrimState m) f a -> Handle (PrimState m) -> Int -> m a
delete seq (Handle hRoot) i = stToPrim $ do
  root <- VGM.unsafeRead hRoot 0
  (!v, !root') <- Seq.deleteST seq root i
  VGM.unsafeWrite hRoot 0 root'
  pure v

-- | Amortized \(O(\log n)\). Frees the \(k\)-th node.
--
-- ==== Constraints
-- - \(0 \le k \lt n\)
--
-- @since 1.2.0.0
{-# INLINE delete_ #-}
delete_ :: (HasCallStack, PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq (PrimState m) f a -> Handle (PrimState m) -> Int -> m ()
delete_ seq (Handle hRoot) i = stToPrim $ do
  VGM.unsafeModifyM
    hRoot
    ( \root -> do
        Seq.deleteST_ seq root i
    )
    0

-- | Amortized \(O(\log n)\). Detaches the \(k\)-th node and returns a handle for it.
--
-- ==== Constraints
-- - \(0 \le k \lt n\)
--
-- @since 1.2.0.0
{-# INLINE detach #-}
detach :: (HasCallStack, PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq (PrimState m) f a -> Handle (PrimState m) -> Int -> m (Handle (PrimState m))
detach seq (Handle hRoot) i = stToPrim $ do
  root <- VGM.unsafeRead hRoot 0
  root' <- Seq.detachST seq root i
  VGM.unsafeWrite hRoot 0 root'
  newHandle root

-- -------------------------------------------------------------------------------------------------
-- Bisection methods
-- -------------------------------------------------------------------------------------------------

-- | Amortized \(O(\log n)\).
--
-- ==== Constraints
-- - The sequence must be non-empty.
--
-- @since 1.2.0.0
{-# INLINE ilowerBound #-}
ilowerBound ::
  (PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) =>
  -- | Sequence storage
  Seq (PrimState m) f a ->
  -- | Sequence handle
  Handle (PrimState m) ->
  -- | User predicate \(f(i, v_i)\) that takes the index and the monoid value
  (Int -> a -> Bool) ->
  -- | Maximum \(r\), where \(f(i, v_i)\) holds for \(i \in [0, r)\)
  m Int
ilowerBound seq (Handle root0) f = stToPrim $ do
  root <- VGM.unsafeRead root0 0
  (!r, !root') <- Seq.ilowerBoundST seq root f
  VGM.unsafeWrite root0 0 root'
  pure r

-- | Amortized \(O(\log n)\).
--
-- ==== Constraints
-- - The sequence must be non-empty.
--
-- @since 1.2.0.0
{-# INLINE ilowerBoundM #-}
ilowerBoundM ::
  (PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) =>
  -- | Sequence storage
  Seq (PrimState m) f a ->
  -- | Sequence handle
  Handle (PrimState m) ->
  -- | User predicate \(f(i, v_i)\) that takes the index and the monoid value
  (Int -> a -> m Bool) ->
  -- | Maximum \(r\), where \(f(i, v_i)\) holds for \(i \in [0, r)\)
  m Int
ilowerBoundM seq (Handle root0) f = do
  root <- VGM.unsafeRead root0 0
  (!r, !root') <- Seq.ilowerBoundM seq root f
  VGM.unsafeWrite root0 0 root'
  pure r

-- | Amortized \(O(\log n)\).
--
-- ==== Constraints
-- - The sequence must be non-empty.
--
-- @since 1.2.0.0
{-# INLINE ilowerBoundProd #-}
ilowerBoundProd ::
  (PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) =>
  -- | Sequence storage
  Seq (PrimState m) f a ->
  -- | Sequence handle
  Handle (PrimState m) ->
  -- | User predicate \(f(i, v_0 \dots v_i)\) that takes the index and the monoid product
  (Int -> a -> Bool) ->
  -- | Maximum \(r\), where \(f(i, v_0 \dots v_i)\) holds for \(i \in [0, r)\)
  m Int
ilowerBoundProd seq (Handle root0) f = stToPrim $ do
  root <- VGM.unsafeRead root0 0
  (!r, !root') <- Seq.ilowerBoundProdST seq root f
  VGM.unsafeWrite root0 0 root'
  pure r

-- | Amortized \(O(\log n)\).
--
-- ==== Constraints
-- - The sequence must be non-empty.
--
-- @since 1.2.0.0
{-# INLINE ilowerBoundProdM #-}
ilowerBoundProdM ::
  (PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) =>
  -- | Sequence storage
  Seq (PrimState m) f a ->
  -- | Sequence handle
  Handle (PrimState m) ->
  -- | User predicate \(f(i, v_0 \dots v_i)\) that takes the index and the monoid product
  (Int -> a -> m Bool) ->
  -- | Maximum \(r\), where \(f(i, v_0 \dots v_i)\) holds for \(i \in [0, r)\)
  m Int
ilowerBoundProdM seq (Handle root0) f = do
  root <- VGM.unsafeRead root0 0
  (!r, !root') <- Seq.ilowerBoundProdM seq root f
  VGM.unsafeWrite root0 0 root'
  pure r

-- | Amortized \(O(\log n)\). Splits a sequence into two with the user predicate and returns the
-- right sequence handle.
--
-- ==== Constraints
-- - The sequence must be non-empty.
--
-- @since 1.2.0.0
{-# INLINE isplitMaxRight #-}
isplitMaxRight ::
  (PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) =>
  -- | Sequence storage
  Seq (PrimState m) f a ->
  -- | Sequence handle
  Handle (PrimState m) ->
  -- | User predicate \(f(i, v_i)\) that takes the index and the monoid value
  (Int -> a -> Bool) ->
  -- | Handle of the right sequence \([r, n)\), where \(r\) is the maximum \(r\) such that
  -- \(f(i, v_i)\) holds for \(i \in [0, r)\)
  m (Handle (PrimState m))
isplitMaxRight seq (Handle root0) f = stToPrim $ do
  root <- VGM.unsafeRead root0 0
  (!l, !r) <- Seq.isplitMaxRightST seq root f
  VGM.unsafeWrite root0 0 l
  newHandle r

-- | Amortized \(O(\log n)\). Splits a sequence into two with the user predicate and returns the
-- right sequence handle.
--
-- ==== Constraints
-- - The sequence must be non-empty.
--
-- @since 1.2.0.0
{-# INLINEABLE isplitMaxRightM #-}
isplitMaxRightM ::
  (PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) =>
  -- | Sequence storage
  Seq (PrimState m) f a ->
  -- | Sequence handle
  Handle (PrimState m) ->
  -- | User predicate \(f(i, v_i)\) that takes the index and the monoid value
  (Int -> a -> m Bool) ->
  -- | Handle of the right sequence \([r, n)\), where \(r\) is the maximum \(r\) such that
  -- \(f(i, v_i)\) holds for \(i \in [0, r)\)
  m (Handle (PrimState m))
isplitMaxRightM seq (Handle root0) f = do
  root <- VGM.unsafeRead root0 0
  (!l, !r) <- Seq.isplitMaxRightM seq root f
  VGM.unsafeWrite root0 0 l
  newHandle r

-- | Amortized \(O(\log n)\). Splits a sequence into two with the user predicate and returns the
-- right sequence handle.
--
-- ==== Constraints
-- - The sequence must be non-empty.
--
-- @since 1.2.0.0
{-# INLINE isplitMaxRightProd #-}
isplitMaxRightProd ::
  (PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) =>
  -- | Sequence storage
  Seq (PrimState m) f a ->
  -- | Sequence handle
  Handle (PrimState m) ->
  -- | User predicate \(f(i, v_0 \dots v_i)\) that takes the index and the monoid value
  (Int -> a -> Bool) ->
  -- | Handle of the right sequence \([r, n)\), where \(r\) is the maximum \(r\) such that
  -- \(f(i, v_0 \dots v_i)\) holds for \(i \in [0, r)\)
  m (Handle (PrimState m))
isplitMaxRightProd seq (Handle root0) f = stToPrim $ do
  root <- VGM.unsafeRead root0 0
  (!l, !r) <- Seq.isplitMaxRightProdST seq root f
  VGM.unsafeWrite root0 0 l
  newHandle r

-- | Amortized \(O(\log n)\). Splits a sequence into two with the user predicate and returns the
-- right sequence handle.
--
-- ==== Constraints
-- - The sequence must be non-empty.
--
-- @since 1.2.0.0
{-# INLINEABLE isplitMaxRightProdM #-}
isplitMaxRightProdM ::
  (PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) =>
  -- | Sequence storage
  Seq (PrimState m) f a ->
  -- | Sequence handle
  Handle (PrimState m) ->
  -- | User predicate \(f(i, v_0 \dots v_i)\) that takes the index and the monoid value
  (Int -> a -> m Bool) ->
  -- | Handle of the right sequence \([r, n)\), where \(r\) is the maximum \(r\) such that
  -- \(f(i, v_0 \dots v_i)\) holds for \(i \in [0, r)\)
  m (Handle (PrimState m))
isplitMaxRightProdM seq (Handle root0) f = do
  root <- VGM.unsafeRead root0 0
  (!l, !r) <- Seq.isplitMaxRightProdM seq root f
  VGM.unsafeWrite root0 0 l
  newHandle r

-- -------------------------------------------------------------------------------------------------
-- Conversions
-- -------------------------------------------------------------------------------------------------

-- | Amortized \(O(n)\). Returns the sequence of monoid values.
--
-- @since 1.2.0.0
{-# INLINEABLE freeze #-}
freeze :: (HasCallStack, PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq (PrimState m) f a -> Handle (PrimState m) -> m (VU.Vector a)
freeze seq (Handle hRoot) = stToPrim $ do
  root <- VGM.unsafeRead hRoot 0
  Seq.freezeST seq root
