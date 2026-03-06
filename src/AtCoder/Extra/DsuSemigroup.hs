{-# LANGUAGE RecordWildCards #-}

-- | A disjoint set union with commutative semigroup values associated with each group.
--
-- ==== __Example__
--
-- >>> import AtCoder.Extra.DsuSemigroup qualified as Dm
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
-- @since 1.6.0.0
module AtCoder.Extra.DsuSemigroup
  ( -- * Disjoint set union
    DsuSemigroup (dsuDm, mDm),

    -- * Constructors
    new,
    build,

    -- * Merging
    merge,
    mergeMaybe,
    merge_,

    -- * Leader
    leader,
    isLeader,

    -- * Component information
    same,
    size,
    groups,

    -- * Semigroup values
    read,
    write,
    modify,
    modifyM,
    unsafeRead,
    unsafeWrite,
    unsafeModify,
    unsafeModifyM,
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

-- | A disjoint set union with commutative semigroup values associated with each group.
--
-- @since 1.6.0.0
data DsuSemigroup s a = DsuSemigroup
  { -- | The original DSU.
    --
    -- @since 1.6.0.0
    dsuDm :: {-# UNPACK #-} !(Dsu.Dsu s),
    -- | Commutative semigroup values for each group.
    --
    -- @since 1.6.0.0
    mDm :: !(VUM.MVector s a)
  }

-- | Creates an undirected graph with \(n\) vertices and \(0\) edges. Requires @Monoid@ constraint.
--
-- ==== Constraints
-- - \(0 \le n\)
--
-- ==== Complexity
-- - \(O(n)\)
--
-- @since 1.6.0.0
{-# INLINE new #-}
new :: (PrimMonad m, Monoid a, VU.Unbox a) => Int -> m (DsuSemigroup (PrimState m) a)
new n
  | n >= 0 = build $ VU.replicate n mempty
  | otherwise = error $ "AtCoder.Extra.DsuSemigroup: given negative size (`" ++ show n ++ "`)"

-- | Creates an undirected graph with \(n\) vertices and \(0\) edges.
--
-- ==== Constraints
-- - \(0 \le n\)
--
-- ==== Complexity
-- - \(O(n)\)
--
-- @since 1.6.0.0
{-# INLINE build #-}
build :: (PrimMonad m, Semigroup a, VU.Unbox a) => VU.Vector a -> m (DsuSemigroup (PrimState m) a)
build ms = stToPrim $ do
  dsuDm <- Dsu.new $ VU.length ms
  mDm <- VU.thaw ms
  pure $ DsuSemigroup {..}

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
-- @since 1.6.0.0
{-# INLINEABLE merge #-}
merge :: (HasCallStack, PrimMonad m, Semigroup a, VU.Unbox a) => DsuSemigroup (PrimState m) a -> Int -> Int -> m Int
merge DsuSemigroup {..} a b = stToPrim $ do
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
-- @since 1.6.0.0
{-# INLINEABLE mergeMaybe #-}
mergeMaybe :: (HasCallStack, PrimMonad m, Semigroup a, VU.Unbox a) => DsuSemigroup (PrimState m) a -> Int -> Int -> m (Maybe Int)
mergeMaybe DsuSemigroup {..} a b = stToPrim $ do
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
-- @since 1.6.0.0
{-# INLINE merge_ #-}
merge_ :: (PrimMonad m, Semigroup a, VU.Unbox a) => DsuSemigroup (PrimState m) a -> Int -> Int -> m ()
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
-- @since 1.6.0.0
{-# INLINE same #-}
same :: (HasCallStack, PrimMonad m) => DsuSemigroup (PrimState m) a -> Int -> Int -> m Bool
same dsu = Dsu.same (dsuDm dsu)

-- | Returns the representative of the connected component that contains the vertex \(a\).
--
-- ==== Constraints
-- - \(0 \leq a \lt n\)
--
-- ==== Complexity
-- - \(O(\alpha(n))\) amortized
--
-- @since 1.6.0.0
{-# INLINE leader #-}
leader :: (HasCallStack, PrimMonad m) => DsuSemigroup (PrimState m) a -> Int -> m Int
leader dsu = Dsu.leader (dsuDm dsu)

-- | Returns whether the vertex \(a\) is the representative of the connected component.
--
-- ==== Constraints
-- - \(0 \leq a \lt n\)
--
-- ==== Complexity
-- - \(O(\alpha(n))\) amortized
--
-- @since 1.6.0.0
{-# INLINE isLeader #-}
isLeader :: (HasCallStack, PrimMonad m) => DsuSemigroup (PrimState m) a -> Int -> m Bool
isLeader dsu v = do
  l <- Dsu.leader (dsuDm dsu) v
  pure $ l == v

-- | Returns the size of the connected component that contains the vertex \(a\).
--
-- ==== Constraints
-- -  \(0 \leq a < n\)
--
-- ==== Complexity
-- - \(O(\alpha(n))\)
--
-- @since 1.6.0.0
{-# INLINE size #-}
size :: (HasCallStack, PrimMonad m) => DsuSemigroup (PrimState m) a -> Int -> m Int
size dsu = Dsu.size (dsuDm dsu)

-- | \O(n)\) Divides the graph into connected components and returns the vector of them.
--
-- More precisely, it returns a vector of the "vector of the vertices in a connected component".
-- Both of the orders of the connected components and the vertices are undefined.
--
-- @since 1.6.0.0
{-# INLINE groups #-}
groups :: (PrimMonad m) => DsuSemigroup (PrimState m) a -> m (V.Vector (VU.Vector Int))
groups dsu = Dsu.groups (dsuDm dsu)

-- | \(O(1)\) Reads the group value of the \(k\)-th node. \(k\) is automatically resolved to the
-- leader vertex.
--
-- @since 1.6.0.0
{-# INLINE read #-}
read :: (PrimMonad m, VU.Unbox a) => DsuSemigroup (PrimState m) a -> Int -> m a
read DsuSemigroup {..} i = do
  VGM.read mDm =<< Dsu.leader dsuDm i

-- | \(O(1)\) Writes to the group value of the \(k\)-th node. \(k\) is automatically resolved to the
-- leader vertex.
--
-- @since 1.6.0.0
{-# INLINE write #-}
write :: (PrimMonad m, VU.Unbox a) => DsuSemigroup (PrimState m) a -> Int -> a -> m ()
write DsuSemigroup {..} i x = do
  i' <- Dsu.leader dsuDm i
  VGM.write mDm i' x

-- | \(O(1)\) Modifies the group value of the \(k\)-th node. \(k\) is automatically resolved to the
-- leader vertex.
--
-- @since 1.6.0.0
{-# INLINE modify #-}
modify :: (PrimMonad m, VU.Unbox a) => DsuSemigroup (PrimState m) a -> (a -> a) -> Int -> m ()
modify DsuSemigroup {..} f i = do
  i' <- Dsu.leader dsuDm i
  VGM.modify mDm f i'

-- | \(O(1)\) Modifies the group value of the \(k\)-th node. \(k\) is automatically resolved to the
-- leader vertex.
--
-- @since 1.6.0.0
{-# INLINE modifyM #-}
modifyM :: (PrimMonad m, VU.Unbox a) => DsuSemigroup (PrimState m) a -> (a -> m a) -> Int -> m ()
modifyM DsuSemigroup {..} f i = do
  i' <- Dsu.leader dsuDm i
  VGM.modifyM mDm f i'

-- | \(O(1)\) Reads the \(k\)-th node.
--
-- @since 1.6.0.0
{-# INLINE unsafeRead #-}
unsafeRead :: (PrimMonad m, VU.Unbox a) => DsuSemigroup (PrimState m) a -> Int -> m a
unsafeRead DsuSemigroup {..} i = do
  VGM.read mDm i

-- | \(O(1)\) Writes to the \(k\)-th node.
--
-- @since 1.6.0.0
{-# INLINE unsafeWrite #-}
unsafeWrite :: (PrimMonad m, VU.Unbox a) => DsuSemigroup (PrimState m) a -> Int -> a -> m ()
unsafeWrite DsuSemigroup {..} i x = do
  VGM.write mDm i x

-- | \(O(1)\) Modifies the value of the \(k\)-th node.
--
-- @since 1.6.0.0
{-# INLINE unsafeModify #-}
unsafeModify :: (PrimMonad m, VU.Unbox a) => DsuSemigroup (PrimState m) a -> (a -> a) -> Int -> m ()
unsafeModify DsuSemigroup {..} f i = do
  VGM.modify mDm f i

-- | \(O(1)\) Modifies the value of the \(k\)-th node.
--
-- @since 1.6.0.0
{-# INLINE unsafeModifyM #-}
unsafeModifyM :: (PrimMonad m, VU.Unbox a) => DsuSemigroup (PrimState m) a -> (a -> m a) -> Int -> m ()
unsafeModifyM DsuSemigroup {..} f i = do
  VGM.modifyM mDm f i
