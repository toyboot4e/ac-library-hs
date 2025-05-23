{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}

-- | A dynamic, persistent, lazily propagated segment tree that covers a half-open interval
-- \([l_0, r_0)\). Nodes are instantiated as needed, with the required capacity being
-- /approximately/ \(8q \log_2 L\), where \(q\) is the number of mutable operations and \(L\) is the
-- length of the interval.
--
-- ==== __Example__
--
-- >>> import AtCoder.Extra.DynLazySegTree.Persistent qualified as Seg
-- >>> import AtCoder.Extra.Monoid.Affine1 (Affine1 (..))
-- >>> import AtCoder.Extra.Monoid.Affine1 qualified as Affine1
-- >>> import Data.Semigroup (Sum (..))
-- >>> import Data.Vector.Unboxed qualified as VU
--
-- Create a `DynLazySegTree` over \([0, 4)\) with some initial capacity:
--
-- >>> let len = 4; q = 3
-- >>> seg <- Seg.new @_ @(Affine1 Int) @(Sum Int) (Seg.recommendedCapacity len q) 0 4
--
-- Different from the @LazySegTree@ module, it requires explicit root handle:
--
-- >>> -- [0, 0, 0, 0]
-- >>> root <- Seg.newRoot seg
--
-- Each modification returns a new handle:
--
-- >>> root1 <- Seg.write seg root 1 $ Sum 10
-- >>> root2 <- Seg.write seg root1 2 $ Sum 20
-- >>> -- [0, 10, 20, 0]
-- >>> Seg.prod seg root2 0 3
-- Sum {getSum = 30}
--
-- >>> -- [0, 10, 20, 0] -> [0, 21, 41, 1]
-- >>> root3 <- Seg.applyIn seg root2 1 4 $ Affine1.new 2 1
-- >>> Seg.maxRight seg root3 (<= (Sum 62))
-- 3
--
-- If multiple tree roots are allocated, `copyInterval` and `copyIntervalWith` can be used.
--
-- @since 1.2.1.0
module AtCoder.Extra.DynLazySegTree.Persistent
  ( -- * Dynamic, lazily propagated segment tree
    Raw.DynLazySegTree (..),

    -- * Re-exports
    SegAct (..),
    P.Index (..),

    -- * Constructors
    new,
    buildWith,
    recommendedCapacity,
    newRoot,
    newSeq,

    -- * Accessing elements
    write,
    modify,
    modifyM,
    -- exchange,
    -- read,

    -- * Products
    prod,
    -- prodMaybe,
    allProd,

    -- * Applications
    applyAt,
    applyIn,
    applyAll,

    -- * Tree operations
    copyInterval,
    copyIntervalWith,
    resetInterval,

    -- * Binary searches
    maxRight,
    maxRightM,
    -- -- * Conversions
    -- freeze,

    -- * Clear
    clear,
  )
where

import AtCoder.Extra.DynLazySegTree.Raw qualified as Raw
import AtCoder.Extra.Pool qualified as P
import AtCoder.LazySegTree (SegAct (..))
import Control.Monad.Primitive (PrimMonad, PrimState, stToPrim)
import Data.Vector.Unboxed qualified as VU
import GHC.Stack (HasCallStack)
import Prelude hiding (read)

-- | \(O(n)\) Creates a `DynLazySegTree` of capacity \(n\) for interval \([l_0, r_0)\) with `mempty`
-- as initial leaf values.
--
-- @since 1.2.1.0
{-# INLINE new #-}
new ::
  (HasCallStack, PrimMonad m, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) =>
  -- | Capacity \(n\)
  Int ->
  -- | Left index boundary \(l_0\)
  Int ->
  -- | Right index boundary \(r_0\)
  Int ->
  -- | Dynamic, persistent, lazily propagated segment tree
  m (Raw.DynLazySegTree (PrimState m) f a)
new capacityLdst l r = stToPrim $ Raw.newST True capacityLdst l r (\_ _ -> mempty)

-- | \(O(n)\) Creates a `DynLazySegTree` of capacity \(n\) for interval \([l_0, r_0)\) with initial
-- value assignment \(g(l, r)\).
--
-- @since 1.2.1.0
{-# INLINE buildWith #-}
buildWith ::
  (HasCallStack, PrimMonad m, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) =>
  -- | Capacity \(n\)
  Int ->
  -- | Left index boundary \(l_0\)
  Int ->
  -- | Right index boundary \(r_0\)
  Int ->
  -- | Initial monoid value assignment \(g: (l, r) \rightarrow a\)
  (Int -> Int -> a) ->
  -- | Dynamic, persistent, lazily propagated segment tree
  m (Raw.DynLazySegTree (PrimState m) f a)
buildWith capacityLdst l r g = stToPrim $ Raw.newST True capacityLdst l r g

-- | \(O(1)\) Returns recommended capacity for \(L\) and \(q\): about \(8q \log_2 L\).
--
-- @since 1.2.1.0
{-# INLINE recommendedCapacity #-}
recommendedCapacity :: Int -> Int -> Int
recommendedCapacity n q = 8 * q * max 2 (ceiling (logBase 2 (fromIntegral n) :: Double))

-- | \(O(1)\) Creates a new root in \([l_0, r_0)\).
--
-- @since 1.2.1.0
newRoot :: (HasCallStack, PrimMonad m, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Raw.DynLazySegTree (PrimState m) f a -> m P.Index
newRoot dst = stToPrim $ Raw.newRootST dst

-- | \(O(n)\) Creates a new root node with contiguous leaf values.
--
-- ==== Constraints
-- - \([l_0, r_0) = [0, L)\): The index boundary of the segment tree must match the sequence.
--
-- @since 1.2.1.0
newSeq :: (HasCallStack, PrimMonad m, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Raw.DynLazySegTree (PrimState m) f a -> VU.Vector a -> m P.Index
newSeq dst xs = stToPrim $ Raw.newSeqST dst xs

-- | \(O(\log L)\) Writes to the monoid value of the node at \(i\).
--
-- ==== Constraints
-- - \(l_0 \le i \lt r_0\)
--
-- @since 1.2.1.0
{-# INLINE write #-}
write :: (HasCallStack, PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Raw.DynLazySegTree (PrimState m) f a -> P.Index -> Int -> a -> m P.Index
write dst root i x = stToPrim $ do
  Raw.modifyMST dst root (pure . const x) i

-- | \(O(\log L)\) Modifies the monoid value of the node at \(i\).
--
-- ==== Constraints
-- - \(l_0 \le i \lt r_0\)
--
-- @since 1.2.1.0
{-# INLINE modify #-}
modify :: (HasCallStack, PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Raw.DynLazySegTree (PrimState m) f a -> P.Index -> (a -> a) -> Int -> m P.Index
modify dst root f i = stToPrim $ do
  Raw.modifyMST dst root (pure . f) i

-- | \(O(\log L)\) Modifies the monoid value of the node at \(i\).
--
-- ==== Constraints
-- - \(l_0 \le i \lt r_0\)
--
-- @since 1.2.1.0
{-# INLINE modifyM #-}
modifyM :: (HasCallStack, PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Raw.DynLazySegTree (PrimState m) f a -> P.Index -> (a -> m a) -> Int -> m P.Index
modifyM dst root f i = do
  Raw.modifyMST dst root f i

-- | \(O(\log L)\) Returns the monoid product in \([l, r)\).
--
-- ==== Constraints
-- - \(l_0 \le l \le r \le r_0\)
--
-- @since 1.2.1.0
{-# INLINE prod #-}
prod :: (HasCallStack, PrimMonad m, SegAct f a, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Raw.DynLazySegTree (PrimState m) f a -> P.Index -> Int -> Int -> m a
prod dst root l r = stToPrim $ do
  Raw.prodST dst root l r

-- | \(O(\log L)\) Returns the monoid product in \([l_0, r_0)\).
--
-- @since 1.2.1.0
{-# INLINE allProd #-}
allProd :: (HasCallStack, PrimMonad m, SegAct f a, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Raw.DynLazySegTree (PrimState m) f a -> P.Index -> m a
allProd dst@Raw.DynLazySegTree {l0Ldst, r0Ldst} root = stToPrim $ do
  Raw.prodST dst root l0Ldst r0Ldst

-- | \(O(\log L)\) Applies a monoid action \(f\) to the node at index \(i\).
--
-- ==== Constraints
-- - \(l_0 \le i \lt r_0\)
-- - The root is not null
--
-- @since 1.2.1.0
{-# INLINE applyAt #-}
applyAt :: (HasCallStack, PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Raw.DynLazySegTree (PrimState m) f a -> P.Index -> Int -> f -> m P.Index
applyAt dst root i act = stToPrim $ do
  Raw.applyInST dst root i (i + 1) act

-- | \(O(\log L)\) Applies a monoid action \(f\) to an interval \([l, r)\).
--
-- ==== Constraints
-- - \(l_0 \le l \le r \le r_0\)
-- - The root is not null
--
-- @since 1.2.1.0
{-# INLINE applyIn #-}
applyIn :: (HasCallStack, PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Raw.DynLazySegTree (PrimState m) f a -> P.Index -> Int -> Int -> f -> m P.Index
applyIn dst root l r act = stToPrim $ do
  Raw.applyInST dst root l r act

-- | \(O(\log L)\) Applies a monoid action \(f\) to the interval \([l_0, r_0)\).
--
-- @since 1.2.1.0
{-# INLINE applyAll #-}
applyAll :: (HasCallStack, PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Raw.DynLazySegTree (PrimState m) f a -> P.Index -> f -> m P.Index
applyAll dst@Raw.DynLazySegTree {l0Ldst, r0Ldst} root act = stToPrim $ do
  Raw.applyInST dst root l0Ldst r0Ldst act

-- | \(O(\log L)\) Given two trees \(a\) and \(b\), copies \(b[l, r)\) to \(a[l, r)\).
--
-- ==== Constraints
-- - \(l_0 \le l \le r \le r_0\)
--
-- @since 1.2.1.0
{-# INLINE copyInterval #-}
copyInterval :: (HasCallStack, PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Raw.DynLazySegTree (PrimState m) f a -> P.Index -> P.Index -> Int -> Int -> m P.Index
copyInterval dst root other l r = stToPrim $ do
  Raw.copyIntervalWithST dst root other l r mempty

-- | \(O(\log L)\) Given two trees \(a\) and \(b\), copies \(b[l, r)\) to \(a[l, r)\), applying a
-- monoid action \(f\).
--
-- ==== Constraints
-- - \(l_0 \le l \le r \le r_0\)
--
-- @since 1.2.1.0
{-# INLINE copyIntervalWith #-}
copyIntervalWith :: (HasCallStack, PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Raw.DynLazySegTree (PrimState m) f a -> P.Index -> P.Index -> Int -> Int -> f -> m P.Index
copyIntervalWith dst root other l r act = stToPrim $ do
  Raw.copyIntervalWithST dst root other l r act

-- | \(O(\log L)\) Resets an interval \([l, r)\) to initial monoid values.
--
-- ==== Constraints
-- - \(l_0 \le l \le r \le r_0\)
--
-- @since 1.2.1.0
{-# INLINE resetInterval #-}
resetInterval ::
  (HasCallStack, PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) =>
  Raw.DynLazySegTree (PrimState m) f a ->
  P.Index ->
  Int ->
  Int ->
  m P.Index
resetInterval dst root l r = stToPrim $ do
  Raw.resetIntervalST dst root l r

-- | \(O(\log L)\) Returns the maximum \(r \in [l_0, r_0)\) where \(f(a_{l_0} a_{l_0 + 1} \dots a_{r - 1})\) holds.
--
-- @since 1.2.1.0
{-# INLINE maxRight #-}
maxRight :: (HasCallStack, PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Raw.DynLazySegTree (PrimState m) f a -> P.Index -> (a -> Bool) -> m Int
maxRight dst root f = do
  Raw.maxRightM dst root (pure . f)

-- | \(O(\log L)\) Returns the maximum \(r \in [l_0, r_0)\) where \(f(a_{l_0} a_{l_0 + 1} \dots a_{r - 1})\) holds.
--
-- @since 1.2.1.0
{-# INLINE maxRightM #-}
maxRightM :: (HasCallStack, PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Raw.DynLazySegTree (PrimState m) f a -> P.Index -> (a -> m Bool) -> m Int
maxRightM dst root f = do
  Raw.maxRightM dst root f

-- | \(O(\log L)\) Clears all the nodes from the storage.
--
-- @since 1.2.2.0
{-# INLINE clear #-}
clear :: (PrimMonad m) => Raw.DynLazySegTree (PrimState m) f a -> m ()
clear dst = do
  P.clear (Raw.poolLdst dst)
