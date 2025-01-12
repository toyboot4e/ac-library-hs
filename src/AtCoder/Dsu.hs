{-# LANGUAGE RecordWildCards #-}

-- | A disjoint set union, also known as a Union-Find tree. It processes the following queries in
-- amortized \(O(\alpha(n))\) time.
--
-- - Edge addition (`merge`)
-- - Deciding whether given two vertices are in the same connected component (`same`)
--
-- Each connected component internally has a representative vertex (`leader`). When two connected
-- components are merged by edge addition (`merge`), one of the two representatives of these
-- connected components becomes the representative (`leader`) of the new connected component.
--
-- ==== __Example__
-- Create a `Dsu` with four vertices:
--
-- >>> import AtCoder.Dsu qualified as Dsu
-- >>> dsu <- Dsu.new 4   -- 0 1 2 3
-- >>> Dsu.nDsu dsu
-- 4
--
-- Merge some vertices into the same group:
--
-- >>> Dsu.merge dsu 0 1  -- 0=1 2 3
-- 0
--
-- >>> Dsu.merge_ dsu 1 2 -- 0=1=2 3
--
-- `leader` returns the internal representative vertex of the connected components:
--
-- >>> Dsu.leader dsu 2
-- 0
--
-- Retrieve group information:
--
-- >>> Dsu.same dsu 0 2
-- True
--
-- >>> Dsu.size dsu 0
-- 3
--
-- >>> Dsu.groups dsu
-- [[2,1,0],[3]]
--
-- @since 1.0.0.0
module AtCoder.Dsu
  ( -- * Disjoint set union
    Dsu (nDsu),

    -- * Constructor
    new,

    -- * Merging
    merge,
    merge_,

    -- * Leader
    leader,

    -- * Component information
    same,
    size,
    groups,
  )
where

import AtCoder.Internal.Assert qualified as ACIA
import Control.Monad (when)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Vector qualified as V
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Stack (HasCallStack)

-- | A disjoint set union. Akso known as Union-Find tree.
--
-- @since 1.0.0.0
data Dsu s = Dsu
  { -- | The number of nodes.
    --
    -- @since 1.0.0.0
    nDsu :: {-# UNPACK #-} !Int,
    -- | For root (leader) nodes it stores their size as a negative number. For child nodes it
    -- stores their parent node index.
    parentOrSizeDsu :: !(VUM.MVector s Int)
  }

-- | Creates an undirected graph with \(n\) vertices and \(0\) edges.
--
-- ==== Constraints
-- - \(0 \le n\)
--
-- ==== Complexity
-- - \(O(n)\)
--
-- @since 1.0.0.0
{-# INLINE new #-}
new :: (PrimMonad m) => Int -> m (Dsu (PrimState m))
new nDsu
  | nDsu >= 0 = do
      parentOrSizeDsu <- VUM.replicate nDsu (-1)
      pure Dsu {..}
  | otherwise = error $ "new: given negative size (`" ++ show nDsu ++ "`)"

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
-- @since 1.0.0.0
{-# INLINE merge #-}
merge :: (HasCallStack, PrimMonad m) => Dsu (PrimState m) -> Int -> Int -> m Int
merge dsu@Dsu {..} a b = do
  let !_ = ACIA.checkVertex "AtCoder.Dsu.merge" a nDsu
  let !_ = ACIA.checkVertex "AtCoder.Dsu.merge" b nDsu
  x <- leader dsu a
  y <- leader dsu b
  if x == y
    then do
      pure x
    else do
      px <- VGM.read parentOrSizeDsu x
      py <- VGM.read parentOrSizeDsu y
      when (-px < -py) $ do
        VGM.swap parentOrSizeDsu x y
      sizeY <- VGM.exchange parentOrSizeDsu y x
      VGM.modify parentOrSizeDsu (+ sizeY) x
      pure x

-- | `merge` with the return value discarded.
--
-- ==== Constraints
-- - \(0 \leq a < n\)
-- - \(0 \leq b < n\)
--
-- ==== Complexity
-- - \(O(\alpha(n))\) amortized
--
-- @since 1.0.0.0
{-# INLINE merge_ #-}
merge_ :: (PrimMonad m) => Dsu (PrimState m) -> Int -> Int -> m ()
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
-- @since 1.0.0.0
{-# INLINE same #-}
same :: (HasCallStack, PrimMonad m) => Dsu (PrimState m) -> Int -> Int -> m Bool
same dsu@Dsu {..} a b = do
  let !_ = ACIA.checkVertex "AtCoder.Dsu.same" a nDsu
  let !_ = ACIA.checkVertex "AtCoder.Dsu.same" b nDsu
  la <- leader dsu a
  lb <- leader dsu b
  pure $ la == lb

-- | Returns the representative of the connected component that contains the vertex \(a\).
--
-- ==== Constraints
-- - \(0 \leq a \lt n\)
--
-- ==== Complexity
-- - \(O(\alpha(n))\) amortized
--
-- @since 1.0.0.0
{-# INLINE leader #-}
leader :: (HasCallStack, PrimMonad m) => Dsu (PrimState m) -> Int -> m Int
leader dsu@Dsu {..} a = do
  let !_ = ACIA.checkVertex "AtCoder.Dsu.leader" a nDsu
  pa <- VGM.read parentOrSizeDsu a
  if pa < 0
    then pure a
    else do
      lpa <- leader dsu pa
      VGM.write parentOrSizeDsu a lpa
      pure lpa

-- | Returns the size of the connected component that contains the vertex \(a\).
--
-- ==== Constraints
-- -  \(0 \leq a < n\)
--
-- ==== Complexity
-- - \(O(\alpha(n))\)
--
-- @since 1.0.0.0
{-# INLINE size #-}
size :: (HasCallStack, PrimMonad m) => Dsu (PrimState m) -> Int -> m Int
size dsu@Dsu {..} a = do
  let !_ = ACIA.checkVertex "AtCoder.Dsu.size" a nDsu
  la <- leader dsu a
  sizeLa <- VGM.read parentOrSizeDsu la
  pure (-sizeLa)

-- | Divides the graph into connected components and returns the vector of them.
--
-- More precisely, it returns a vector of the "vector of the vertices in a connected component".
-- Both of the orders of the connected components and the vertices are undefined.
--
-- ==== Complexity
-- - \(O(n)\)
--
-- @since 1.0.0.0
{-# INLINE groups #-}
groups :: (PrimMonad m) => Dsu (PrimState m) -> m (V.Vector (VU.Vector Int))
groups dsu@Dsu {..} = do
  groupSize <- VUM.replicate nDsu (0 :: Int)
  leaders <- VU.generateM nDsu $ \i -> do
    li <- leader dsu i
    VGM.modify groupSize (+ 1) li
    pure li
  result <- do
    groupSize' <- VU.unsafeFreeze groupSize
    V.mapM VUM.unsafeNew $ VU.convert groupSize'
  VU.iforM_ leaders $ \i li -> do
    i' <- subtract 1 <$> VGM.read groupSize li
    VGM.write (result VG.! li) i' i
    VGM.write groupSize li i'
  V.filter (not . VU.null) <$> V.mapM VU.unsafeFreeze result
