{-# LANGUAGE TypeFamilies #-}

-- | A dynamic, sparse segment tree that covers a half-open interval \([l_0, r_0)\). The nodes are
-- instantinated as needed. The required capacity is /approximately/ \(2q \log L\), where \(q\) is
-- the number of mutable operations and \(L\) is the length of the interval.
--
-- ==== __Example__
--
-- @since 1.2.1.0
module AtCoder.Extra.DynSparseSegTree.Persistent
  ( -- * Dynamic, sparse segment tree
    Raw.DynSparseSegTree (..),

    -- * Re-exports
    P.Handle (..),

    -- * Constructors
    new,
    recommendedCapacity,
    newRoot,

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

    -- * Binary searches
    maxRight,
    maxRightM,
    -- -- * Conversions
    -- freeze,
  )
where

import AtCoder.Extra.DynSparseSegTree.Raw qualified as Raw
import AtCoder.Extra.Pool qualified as P
import Control.Monad.Primitive (PrimMonad, PrimState, stToPrim)
import Data.Vector.Unboxed qualified as VU
import GHC.Stack (HasCallStack)
import Prelude hiding (read)

-- | \(O(n)\) Creates a `DynSparseSegTree` of capacity \(n\) for interval \([l_0, r_0)\) with
-- `mempty` as initial leaf values.
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
  -- | Dynamic, sparse segment tree
  m (Raw.DynSparseSegTree (PrimState m) a)
new nDsst l r = stToPrim $ Raw.newST True nDsst l r

-- | \(O(1)\) Returns recommended capacity for length \(l\) and the number of modfication queries
-- \(q\).
--
-- @since 1.2.1.0
{-# INLINE recommendedCapacity #-}
recommendedCapacity :: Int -> Int -> Int
recommendedCapacity _ q = 2 * q

-- | \(O(1)\) Creates a new root in \([l_0, r_0)\).
--
-- @since 1.2.1.0
newRoot :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => Raw.DynSparseSegTree (PrimState m) a -> m P.Index
newRoot dst = stToPrim $ Raw.newRootST dst

-- | \(O(\log L)\) Writes to the monoid value of node at \(i\).
--
-- ==== Constraints
-- - \(l_0 \le i \lt r_0\)
--
-- @since 1.2.1.0
{-# INLINE write #-}
write :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => Raw.DynSparseSegTree (PrimState m) a -> P.Index -> Int -> a -> m P.Index
write dst root i x = stToPrim $ do
     Raw.modifyMST dst root (pure . const x) i

-- | \(O(\log L)\) Modifies the monoid value of node at \(i\).
--
-- ==== Constraints
-- - \(l_0 \le i \lt r_0\)
--
-- @since 1.2.1.0
{-# INLINE modify #-}
modify :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => Raw.DynSparseSegTree (PrimState m) a -> P.Index -> (a -> a) -> Int -> m P.Index
modify dst root f i = stToPrim $ do
     Raw.modifyMST dst root (pure . f) i

-- | \(O(\log L)\) Modifies the monoid value of node at \(i\).
--
-- ==== Constraints
-- - \(l_0 \le i \lt r_0\)
--
-- @since 1.2.1.0
{-# INLINE modifyM #-}
modifyM :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => Raw.DynSparseSegTree (PrimState m) a -> P.Index -> (a -> m a) -> Int -> m P.Index
modifyM dst root f i = do
    Raw.modifyMST dst root f i

-- | \(O(\log L)\) Returns the monoid product in \([l, r)\).
--
-- ==== Constraints
-- - \(l_0 \le l \le r \le r_0\)
--
-- @since 1.2.1.0
{-# INLINE prod #-}
prod :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => Raw.DynSparseSegTree (PrimState m) a -> P.Index -> Int -> Int -> m a
prod dst root l r = stToPrim $ do
  Raw.prodST dst root l r

-- | \(O(\log L)\) Returns the monoid product in \([l_0, r_0)\).
--
-- @since 1.2.1.0
{-# INLINE allProd #-}
allProd :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => Raw.DynSparseSegTree (PrimState m) a -> P.Index -> m a
allProd dst@Raw.DynSparseSegTree {l0Dsst, r0Dsst} root = stToPrim $ do
  Raw.prodST dst root l0Dsst r0Dsst

-- | \(O(\log L)\) Returns the maximum \(r \in [l_0, r_0)\) where \(f(a_{l_0} a_{l_0 + 1} \dots a_{r - 1})\) holds.
--
-- @since 1.2.1.0
{-# INLINE maxRight #-}
maxRight :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => Raw.DynSparseSegTree (PrimState m) a -> P.Index -> (a -> Bool) -> m Int
maxRight dst root f = do
  Raw.maxRightM dst root (pure . f)

-- | \(O(\log L)\) Returns the maximum \(r \in [l_0, r_0)\) where \(f(a_{l_0} a_{l_0 + 1} \dots a_{r - 1})\) holds.
--
-- @since 1.2.1.0
{-# INLINE maxRightM #-}
maxRightM :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => Raw.DynSparseSegTree (PrimState m) a -> P.Index -> (a -> m Bool) -> m Int
maxRightM dst root f = do
  Raw.maxRightM dst root f
