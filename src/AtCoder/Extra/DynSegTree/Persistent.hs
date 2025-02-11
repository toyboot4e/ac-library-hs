{-# LANGUAGE TypeFamilies #-}

-- | A dynamic, persistent segment tree that covers a half-open interval \([l_0, r_0)\). Nodes are
-- instantinated as needed, with the required capacity being /approximately/ \(4q \log_2 L\), where
-- \(q\) is the number of mutable operations and \(L\) is the length of the interval.
--
-- ==== __Example__
--
-- >>> import AtCoder.Extra.DynSegTree.Persistent qualified as Seg
-- >>> import Data.Semigroup (Sum (..))
-- >>> import Data.Vector.Unboxed qualified as VU
--
-- Create a `DynSegTree` over \([0, 4)\) with some initial capacity:
--
-- >>> let len = 4; q = 2
-- >>> seg <- Seg.new @_ @(Sum Int) (Seg.recommendedCapacity len q) 0 4
--
-- Different from the @SegTree@ module, it requires explicit root handle:
--
-- >>> -- [0, 0, 0, 0]
-- >>> root <- Seg.newRoot seg
-- >>> root1 <- Seg.write seg root 1 $ Sum 10
-- >>> root2 <- Seg.write seg root1 2 $ Sum 20
-- >>> -- [0, 10, 20, 0]
-- >>> Seg.prod seg root2 0 3
-- Sum {getSum = 30}
--
-- >>> Seg.maxRight seg root2 (< (Sum 30))
-- 2
--
-- @since 1.2.1.0
module AtCoder.Extra.DynSegTree.Persistent
  ( -- * Dynamic segment tree
    Raw.DynSegTree (..),

    -- * Re-exports
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
    allProd, -- FIXME: rename it to prodAll

    -- * Tree operations
    resetInterval,

    -- * Binary searches
    maxRight,
    maxRightM,
    -- -- * Conversions
    -- freeze,
  )
where

import AtCoder.Extra.DynSegTree.Raw qualified as Raw
import AtCoder.Extra.Pool qualified as P
import Control.Monad.Primitive (PrimMonad, PrimState, stToPrim)
import Data.Vector.Unboxed qualified as VU
import GHC.Stack (HasCallStack)
import Prelude hiding (read)

-- | \(O(n)\) Creates a `DynSegTree` of capacity \(n\) for interval \([l_0, r_0)\) with `mempty` as
-- initial leaf values.
--
-- @since 1.2.1.0
{-# INLINE new #-}
new ::
  (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) =>
  -- | Capacity \(n\)
  Int ->
  -- | Left index boundary \(l_0\)
  Int ->
  -- | Right index boundary \(r_0\)
  Int ->
  -- | Dynamic, persistent segment tree
  m (Raw.DynSegTree (PrimState m) a)
new nDst l r = stToPrim $ Raw.newST True nDst l r (\_ _ -> mempty)

-- | \(O(n)\) Creates a `DynSegTree` of capacity \(n\) for interval \([l_0, r_0)\) with initial
-- value assignment \(g(l, r)\).
--
-- @since 1.2.1.0
{-# INLINE buildWith #-}
buildWith ::
  (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) =>
  -- | Capacity \(n\)
  Int ->
  -- | Left index boundary \(l_0)\)
  Int ->
  -- | Right index boundary \(r_0)\)
  Int ->
  -- | Initial monoid value assignment \(g: (l, r) \rightarrow a\)
  (Int -> Int -> a) ->
  -- | Dynamic, persistent segment tree
  m (Raw.DynSegTree (PrimState m) a)
buildWith nDst l r g = stToPrim $ Raw.newST True nDst l r g

-- | \(O(1)\) Returns recommended capacity for \(L\) and \(q\): \(2q \log_2 L\).
--
-- @since 1.2.1.0
{-# INLINE recommendedCapacity #-}
recommendedCapacity :: Int -> Int -> Int
recommendedCapacity n q = 2 * q * (2 + ceiling (logBase 2 (fromIntegral n) :: Double))

-- | \(O(1)\) Creates a new root in \([l_0, r_0)\).
--
-- @since 1.2.1.0
newRoot :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => Raw.DynSegTree (PrimState m) a -> m P.Index
newRoot dst = stToPrim $ Raw.newRootST dst

-- | \(O(L)\) Creates a new root node with contiguous leaf values. User would want to use a strict
-- segment tree instead.
--
-- ==== Constraints
-- - \([l_0, r_0) = [0, L)\): The index boundary of the segment tree must match the sequence.
--
-- @since 1.2.1.0
newSeq :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => Raw.DynSegTree (PrimState m) a -> VU.Vector a -> m P.Index
newSeq dst xs = stToPrim $ Raw.newSeqST dst xs

-- | \(O(\log L)\) Writes to the monoid value of the node at \(i\).
--
-- ==== Constraints
-- - \(l_0 \le i \lt r_0\)
--
-- @since 1.2.1.0
{-# INLINE write #-}
write :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => Raw.DynSegTree (PrimState m) a -> P.Index -> Int -> a -> m P.Index
write dst root i x = stToPrim $ do
  Raw.modifyMST dst root (pure . const x) i

-- | \(O(\log L)\) Modifies the monoid value of the node at \(i\).
--
-- ==== Constraints
-- - \(l_0 \le i \lt r_0\)
--
-- @since 1.2.1.0
{-# INLINE modify #-}
modify :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => Raw.DynSegTree (PrimState m) a -> P.Index -> (a -> a) -> Int -> m P.Index
modify dst root f i = stToPrim $ do
  Raw.modifyMST dst root (pure . f) i

-- | \(O(\log L)\) Modifies the monoid value of the node at \(i\).
--
-- ==== Constraints
-- - \(l_0 \le i \lt r_0\)
--
-- @since 1.2.1.0
{-# INLINE modifyM #-}
modifyM :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => Raw.DynSegTree (PrimState m) a -> P.Index -> (a -> m a) -> Int -> m P.Index
modifyM dst root f i = do
  Raw.modifyMST dst root f i

-- | \(O(\log L)\) Returns the monoid product in \([l, r)\).
--
-- ==== Constraints
-- - \(l_0 \le l \le r \le r_0\)
--
-- @since 1.2.1.0
{-# INLINE prod #-}
prod :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => Raw.DynSegTree (PrimState m) a -> P.Index -> Int -> Int -> m a
prod dst root l r = stToPrim $ do
  Raw.prodST dst root l r

-- | \(O(\log L)\) Returns the monoid product in \([l_0, r_0)\).
--
-- @since 1.2.1.0
{-# INLINE allProd #-}
allProd :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => Raw.DynSegTree (PrimState m) a -> P.Index -> m a
allProd dst@Raw.DynSegTree {l0Dst, r0Dst} root = stToPrim $ do
  Raw.prodST dst root l0Dst r0Dst

-- | \(O(\log L)\) Resets an interval \([l, r)\) to initial monoid values.
--
-- ==== Constraints
-- - \(l_0 \le l \le r \le r_0\)
--
-- @since 1.2.1.0
{-# INLINE resetInterval #-}
resetInterval :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => Raw.DynSegTree (PrimState m) a -> P.Index -> Int -> Int -> m P.Index
resetInterval dst root l r = stToPrim $ do
  Raw.resetIntervalST dst root l r

-- | \(O(\log L)\) Returns the maximum \(r \in [l_0, r_0)\) where \(f(a_{l_0} a_{l_0 + 1} \dots a_{r - 1})\) holds.
--
-- @since 1.2.1.0
{-# INLINE maxRight #-}
maxRight :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => Raw.DynSegTree (PrimState m) a -> P.Index -> (a -> Bool) -> m Int
maxRight dst root f = do
  Raw.maxRightM dst root (pure . f)

-- | \(O(\log L)\) Returns the maximum \(r \in [l_0, r_0)\) where \(f(a_{l_0} a_{l_0 + 1} \dots a_{r - 1})\) holds.
--
-- @since 1.2.1.0
{-# INLINE maxRightM #-}
maxRightM :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => Raw.DynSegTree (PrimState m) a -> P.Index -> (a -> m Bool) -> m Int
maxRightM dst root f = do
  Raw.maxRightM dst root f
