{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

-- | A dynamic, sparse segment tree that covers a half-open interval \([l_0, r_0)\). Nodes are
-- instantinated as needed, with the required capacity being \(q\), where \(q\) is the number of
-- mutable operations. The traid-off compared to the non-sparse variant is that initial monoid
-- values are fixed at `mempty`.
--
-- ==== __Example__
--
-- >>> import AtCoder.Extra.DynSparseSegTree qualified as Seg
-- >>> import Data.Semigroup (Sum (..))
-- >>> import Data.Vector.Unboxed qualified as VU
--
-- Create a `DynSegTree` over \([0, 4)\) with some initial capacity:
--
-- >>> let capacityFor len q = q * max 2 (2 + ceiling (logBase 2 (fromIntegral len) :: Double))
-- >>> let len = 4; q = 2
-- >>> seg <- Seg.new @_ @(Sum Int) (capacityFor len q) 0 4
--
-- Different from the @SegTree@ module, it requires explicit root handle:
--
-- >>> -- [0, 0, 0, 0]
-- >>> root <- Seg.newRoot seg
-- >>> Seg.write seg root 1 $ Sum 10
-- >>> Seg.write seg root 2 $ Sum 20
-- >>> -- [0, 10, 20, 0]
-- >>> Seg.prod seg root 0 3
-- Sum {getSum = 30}
--
-- >>> Seg.maxRight seg root (< (Sum 30))
-- 2
--
-- @since 1.2.1.0
module AtCoder.Extra.DynSparseSegTree
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
    allProd,

    -- * Binary searches
    maxRight,
    maxRightM,
    -- -- * Conversions
    -- freeze,

    -- * Clear
    clear,
  )
where

import AtCoder.Extra.DynSparseSegTree.Raw qualified as Raw
import AtCoder.Extra.Pool qualified as P
import Control.Monad.Primitive (PrimMonad, PrimState, stToPrim)
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import GHC.Stack (HasCallStack)
import Prelude hiding (read)

-- This module is based on `Handle` because the root is defined as `P.undefIndex`

-- | \(O(n)\) Creates a `DynSparseSegTree` of capacity \(n\) for interval \([l_0, r_0)\) with `mempty` as
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
  -- | Dynamic, sparse segment tree
  m (Raw.DynSparseSegTree (PrimState m) a)
new nDsst l r = stToPrim $ Raw.newST False nDsst l r

-- | \(O(1)\) Returns recommended capacity for \(L\) and \(q\): \(q\).
--
-- @since 1.2.1.0
{-# INLINE recommendedCapacity #-}
recommendedCapacity :: Int -> Int -> Int
recommendedCapacity _ q = q

-- | \(O(1)\) Creates a new root in \([l_0, r_0)\).
--
-- @since 1.2.1.0
newRoot :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => Raw.DynSparseSegTree (PrimState m) a -> m (P.Handle (PrimState m))
newRoot dst = stToPrim $ P.newHandle =<< Raw.newRootST dst

-- | \(O(\log L)\) Writes to the monoid value of the node at \(i\).
--
-- ==== Constraints
-- - \(l_0 \le i \lt r_0\)
--
-- @since 1.2.1.0
{-# INLINE write #-}
write :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => Raw.DynSparseSegTree (PrimState m) a -> P.Handle (PrimState m) -> Int -> a -> m ()
write dst (P.Handle handle) i x = stToPrim $ do
  VGM.modifyM
    handle
    (\root -> Raw.modifyMST dst root (pure . const x) i)
    0

-- | \(O(\log L)\) Modifies the monoid value of the node at \(i\).
--
-- ==== Constraints
-- - \(l_0 \le i \lt r_0\)
--
-- @since 1.2.1.0
{-# INLINE modify #-}
modify :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => Raw.DynSparseSegTree (PrimState m) a -> P.Handle (PrimState m) -> (a -> a) -> Int -> m ()
modify dst (P.Handle handle) f i = stToPrim $ do
  VGM.modifyM
    handle
    (\root -> Raw.modifyMST dst root (pure . f) i)
    0

-- | \(O(\log L)\) Modifies the monoid value of the node at \(i\).
--
-- ==== Constraints
-- - \(l_0 \le i \lt r_0\)
--
-- @since 1.2.1.0
{-# INLINE modifyM #-}
modifyM :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => Raw.DynSparseSegTree (PrimState m) a -> P.Handle (PrimState m) -> (a -> m a) -> Int -> m ()
modifyM dst (P.Handle handle) f i = do
  VGM.modifyM
    handle
    (\root -> Raw.modifyMST dst root f i)
    0

-- | \(O(\log L)\) Returns the monoid product in \([l, r)\).
--
-- ==== Constraints
-- - \(l_0 \le l \le r \le r_0\)
--
-- @since 1.2.1.0
{-# INLINE prod #-}
prod :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => Raw.DynSparseSegTree (PrimState m) a -> P.Handle (PrimState m) -> Int -> Int -> m a
prod dst (P.Handle handle) l r = stToPrim $ do
  root <- VGM.read handle 0
  Raw.prodST dst root l r

-- | \(O(\log L)\) Returns the monoid product in \([l_0, r_0)\).
--
-- @since 1.2.1.0
{-# INLINE allProd #-}
allProd :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => Raw.DynSparseSegTree (PrimState m) a -> P.Handle (PrimState m) -> m a
allProd dst@Raw.DynSparseSegTree {l0Dsst, r0Dsst} (P.Handle handle) = stToPrim $ do
  root <- VGM.read handle 0
  Raw.prodST dst root l0Dsst r0Dsst

-- | \(O(\log L)\) Returns the maximum \(r \in [l_0, r_0)\) where \(f(a_{l_0} a_{l_0 + 1} \dots a_{r - 1})\) holds.
--
-- @since 1.2.1.0
{-# INLINE maxRight #-}
maxRight :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => Raw.DynSparseSegTree (PrimState m) a -> P.Handle (PrimState m) -> (a -> Bool) -> m Int
maxRight dst (P.Handle handle) f = do
  root <- VGM.read handle 0
  Raw.maxRightM dst root (pure . f)

-- | \(O(\log L)\) Returns the maximum \(r \in [l_0, r_0)\) where \(f(a_{l_0} a_{l_0 + 1} \dots a_{r - 1})\) holds.
--
-- @since 1.2.1.0
{-# INLINE maxRightM #-}
maxRightM :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => Raw.DynSparseSegTree (PrimState m) a -> P.Handle (PrimState m) -> (a -> m Bool) -> m Int
maxRightM dst (P.Handle handle) f = do
  root <- VGM.read handle 0
  Raw.maxRightM dst root f

-- | \(O(\log L)\) Claers all the nodes from the storage.
--
-- @since 1.2.2.0
{-# INLINE clear #-}
clear :: (PrimMonad m) => Raw.DynSparseSegTree (PrimState m) a -> m ()
clear dst = do
  P.clear (Raw.poolDsst dst)
