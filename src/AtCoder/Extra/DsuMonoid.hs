{-# LANGUAGE RecordWildCards #-}

-- | A disjoint set union with commutative monoid values associated with each group.
--
-- ==== __Example__
--
-- >>> import AtCoder.Extra.DsuMonoid qualified as Dm
-- >>> import Data.Semigroup (Sum (..))
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> dsu <- Dm.build $ VU.generate 4 Sum
-- >>> Dm.merge dsu 0 1
-- 0
--
-- >>> Dm.read dsu 0
-- Sum {getSum = 1}
--
-- >>> Dm.read dsu 1
-- Sum {getSum = 1}
--
-- >>> Dm.mergeMaybe dsu 0 2
-- Just 0
--
-- >>> Dm.read dsu 0
-- Sum {getSum = 3}
--
-- @since 1.5.3.0
module AtCoder.Extra.DsuMonoid
  ( -- * Disjoint set union
    DsuMonoid (dsuDm, mDm),

    -- * Constructors
    new,
    build,

    -- * Merging
    merge,
    mergeMaybe,
    merge_,

    -- * Leader
    leader,

    -- * Component information
    same,
    size,
    groups,

    -- * Monoid values
    read,
    unsafeRead,
    unsafeWrite,
  )
where

import AtCoder.Dsu qualified as Dsu
import Control.Monad.Primitive (PrimMonad, PrimState, stToPrim)
import Data.Vector qualified as V
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Stack (HasCallStack)
import Prelude hiding (read)

-- | A disjoint set union with commutative monoid values associated with each group.
--
-- @since 1.5.3.0
data DsuMonoid s a = DsuMonoid
  { -- | The original DSU.
    --
    -- @since 1.5.3.0
    dsuDm :: {-# UNPACK #-} !(Dsu.Dsu s),
    -- | Commutative monoid values for each group.
    --
    -- @since 1.5.3.0
    mDm :: !(VUM.MVector s a)
  }

-- | Creates an undirected graph with \(n\) vertices and \(0\) edges.
--
-- ==== Constraints
-- - \(0 \le n\)
--
-- ==== Complexity
-- - \(O(n)\)
--
-- @since 1.5.3.0
{-# INLINE new #-}
new :: (PrimMonad m, Monoid a, VU.Unbox a) => Int -> m (DsuMonoid (PrimState m) a)
new n
  | n >= 0 = build $ VU.replicate n mempty
  | otherwise = error $ "AtCoder.Extra.DsuMonoid: given negative size (`" ++ show n ++ "`)"

-- | Creates an undirected graph with \(n\) vertices and \(0\) edges.
--
-- ==== Constraints
-- - \(0 \le n\)
--
-- ==== Complexity
-- - \(O(n)\)
--
-- @since 1.5.3.0
{-# INLINE build #-}
build :: (PrimMonad m, Semigroup a, VU.Unbox a) => VU.Vector a -> m (DsuMonoid (PrimState m) a)
build ms = stToPrim $ do
  dsuDm <- Dsu.new $ VU.length ms
  mDm <- VU.thaw ms
  pure $ DsuMonoid {..}

-- | Adds an edge \((a, b)\). If the vertices \(a\) and \(b\) are in the same connected component, it
-- returns the representative (`leader`) of this connected component. Otherwise, it returns the
-- representative of the new connected component.
--
-- ==== Constraints
-- - \(0 \leq a < n\)
-- - \(0 \leq b < n\)
--
-- ==== Complexity
-- - \(O(\alpha(n))\) amortized
--
-- @since 1.5.3.0
{-# INLINEABLE merge #-}
merge :: (HasCallStack, PrimMonad m, Semigroup a, VU.Unbox a) => DsuMonoid (PrimState m) a -> Int -> Int -> m Int
merge DsuMonoid {..} a b = stToPrim $ do
  r1 <- Dsu.leader dsuDm a
  r2 <- Dsu.leader dsuDm b
  if r1 == r2
    then pure r1
    else do
      !m1 <- VGM.read mDm r1
      !m2 <- VGM.read mDm r2
      r' <- Dsu.merge dsuDm a b
      VGM.write mDm r' $! m1 <> m2
      pure r'

-- | Adds an edge \((a, b)\). It returns the representative of the new connected component, or
-- `Nothing` if the two vertices are in the same connected component.
--
-- ==== Constraints
-- - \(0 \leq a < n\)
-- - \(0 \leq b < n\)
--
-- ==== Complexity
-- - \(O(\alpha(n))\) amortized
--
-- @since 1.5.3.0
{-# INLINEABLE mergeMaybe #-}
mergeMaybe :: (HasCallStack, PrimMonad m, Semigroup a, VU.Unbox a) => DsuMonoid (PrimState m) a -> Int -> Int -> m (Maybe Int)
mergeMaybe DsuMonoid {..} a b = stToPrim $ do
  r1 <- Dsu.leader dsuDm a
  r2 <- Dsu.leader dsuDm b
  if r1 == r2
    then pure Nothing
    else do
      !m1 <- VGM.read mDm r1
      !m2 <- VGM.read mDm r2
      r' <- Dsu.merge dsuDm a b
      VGM.write mDm r' $! m1 <> m2
      pure $ Just r'

-- | `merge` with the return value discarded.
--
-- ==== Constraints
-- - \(0 \leq a < n\)
-- - \(0 \leq b < n\)
--
-- ==== Complexity
-- - \(O(\alpha(n))\) amortized
--
-- @since 1.5.3.0
{-# INLINE merge_ #-}
merge_ :: (PrimMonad m, Semigroup a, VU.Unbox a) => DsuMonoid (PrimState m) a -> Int -> Int -> m ()
merge_ dsu a b = do
  _ <- merge dsu a b
  pure ()

-- | Returns whether the vertices \(a\) and \(b\) are in the same connected component.
--
-- ==== Constraints
-- - \(0 \leq a < n\)
-- - \(0 \leq b < n\)
--
-- ==== Complexity
-- - \(O(\alpha(n))\) amortized
--
-- @since 1.5.3.0
{-# INLINE same #-}
same :: (HasCallStack, PrimMonad m) => DsuMonoid (PrimState m) a -> Int -> Int -> m Bool
same dsu = Dsu.same (dsuDm dsu)

-- | Returns the representative of the connected component that contains the vertex \(a\).
--
-- ==== Constraints
-- - \(0 \leq a \lt n\)
--
-- ==== Complexity
-- - \(O(\alpha(n))\) amortized
--
-- @since 1.5.3.0
{-# INLINE leader #-}
leader :: (HasCallStack, PrimMonad m) => DsuMonoid (PrimState m) a -> Int -> m Int
leader dsu = Dsu.leader (dsuDm dsu)

-- | Returns the size of the connected component that contains the vertex \(a\).
--
-- ==== Constraints
-- -  \(0 \leq a < n\)
--
-- ==== Complexity
-- - \(O(\alpha(n))\)
--
-- @since 1.5.3.0
{-# INLINE size #-}
size :: (HasCallStack, PrimMonad m) => DsuMonoid (PrimState m) a -> Int -> m Int
size dsu = Dsu.size (dsuDm dsu)

-- | \O(n)\) Divides the graph into connected components and returns the vector of them.
--
-- More precisely, it returns a vector of the "vector of the vertices in a connected component".
-- Both of the orders of the connected components and the vertices are undefined.
--
-- @since 1.5.3.0
{-# INLINE groups #-}
groups :: (PrimMonad m) => DsuMonoid (PrimState m) a -> m (V.Vector (VU.Vector Int))
groups dsu = Dsu.groups (dsuDm dsu)

-- | \(O(1)\) Reads the group value of the \(k\)-th node.
--
-- @since 1.5.3.0
{-# INLINE read #-}
read :: (PrimMonad m, VU.Unbox a) => DsuMonoid (PrimState m) a -> Int -> m a
read DsuMonoid {..} i = do
  VGM.read mDm =<< Dsu.leader dsuDm i

-- | \(O(1)\) Reads the group value of the \(k\)-th node.
--
-- @since 1.5.3.0
{-# INLINE unsafeRead #-}
unsafeRead :: (PrimMonad m, VU.Unbox a) => DsuMonoid (PrimState m) a -> Int -> m a
unsafeRead DsuMonoid {..} i = do
  VGM.read mDm i

-- | \(O(1)\) Writes to the group value of the \(k\)-th node.
--
-- @since 1.5.3.0
{-# INLINE unsafeWrite #-}
unsafeWrite :: (PrimMonad m, VU.Unbox a) => DsuMonoid (PrimState m) a -> Int -> a -> m ()
unsafeWrite DsuMonoid {..} i x = do
  VGM.write mDm i x
