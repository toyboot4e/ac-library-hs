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
-- >>> Dsu.mergeMaybe dsu 0 1 -- already merged
-- Nothing
--
-- >>> Dsu.merge_ dsu 1 2 -- 0=1=2 3
-- >>> Dsu.mergeMaybe dsu 1 2
-- Nothing
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

    -- * Constructors
    new,

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
  )
where

import AtCoder.Internal.Assert qualified as ACIA
import Control.Monad (when)
import Control.Monad.Primitive (PrimMonad, PrimState, stToPrim)
import Control.Monad.ST (ST)
import Data.Vector qualified as V
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Stack (HasCallStack)

-- | A disjoint set union. Also known as Union-Find tree.
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
new = stToPrim . newST

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
merge dsu a b = stToPrim $ mergeST dsu a b

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
-- @since 1.2.4.0
{-# INLINE mergeMaybe #-}
mergeMaybe :: (HasCallStack, PrimMonad m) => Dsu (PrimState m) -> Int -> Int -> m (Maybe Int)
mergeMaybe dsu a b = stToPrim $ mergeMaybeST dsu a b

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
same dsu a b = stToPrim $ sameST dsu a b

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
leader dsu a = stToPrim $ leaderST dsu a

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
isLeader :: (HasCallStack, PrimMonad m) => Dsu (PrimState m) -> Int -> m Bool
isLeader dsu v = do
  l <- leader dsu v
  pure $ l == v

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
size dsu a = stToPrim $ sizeST dsu a

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
groups = stToPrim . groupsST

-- -------------------------------------------------------------------------------------------------
-- Internal
-- -------------------------------------------------------------------------------------------------

{-# INLINEABLE newST #-}
newST :: Int -> ST s (Dsu s)
newST nDsu
  | nDsu >= 0 = do
      parentOrSizeDsu <- VUM.replicate nDsu (-1)
      pure Dsu {..}
  | otherwise = error $ "AtCoder.Dsu.newST: given negative size (`" ++ show nDsu ++ "`)"

{-# INLINEABLE mergeST #-}
mergeST :: (HasCallStack) => Dsu s -> Int -> Int -> ST s Int
mergeST dsu@Dsu {..} a b = do
  let !_ = ACIA.checkVertex "AtCoder.Dsu.mergeST" a nDsu
  let !_ = ACIA.checkVertex "AtCoder.Dsu.mergeST" b nDsu
  x <- leaderST dsu a
  y <- leaderST dsu b
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

{-# INLINEABLE mergeMaybeST #-}
mergeMaybeST :: (HasCallStack) => Dsu s -> Int -> Int -> ST s (Maybe Int)
mergeMaybeST dsu@Dsu {..} a b = do
  let !_ = ACIA.checkVertex "AtCoder.Dsu.mergeMaybeST" a nDsu
  let !_ = ACIA.checkVertex "AtCoder.Dsu.mergeMaybeST" b nDsu
  x <- leaderST dsu a
  y <- leaderST dsu b
  if x == y
    then do
      pure Nothing
    else do
      px <- VGM.read parentOrSizeDsu x
      py <- VGM.read parentOrSizeDsu y
      when (-px < -py) $ do
        VGM.swap parentOrSizeDsu x y
      sizeY <- VGM.exchange parentOrSizeDsu y x
      VGM.modify parentOrSizeDsu (+ sizeY) x
      Just <$> leaderST dsu a

{-# INLINEABLE sameST #-}
sameST :: (HasCallStack) => Dsu s -> Int -> Int -> ST s Bool
sameST dsu@Dsu {..} a b = do
  let !_ = ACIA.checkVertex "AtCoder.Dsu.sameST" a nDsu
  let !_ = ACIA.checkVertex "AtCoder.Dsu.sameST" b nDsu
  la <- leaderST dsu a
  lb <- leaderST dsu b
  pure $ la == lb

-- TODO: INLINE?
{-# INLINEABLE leaderST #-}
leaderST :: Dsu s -> Int -> ST s Int
leaderST dsu@Dsu {..} a = do
  pa <- VGM.read parentOrSizeDsu a
  if pa < 0
    then pure a
    else do
      lpa <- leaderST dsu pa
      VGM.write parentOrSizeDsu a lpa
      pure lpa

{-# INLINEABLE sizeST #-}
sizeST :: (HasCallStack) => Dsu s -> Int -> ST s Int
sizeST dsu@Dsu {..} a = do
  let !_ = ACIA.checkVertex "AtCoder.Dsu.sizeST" a nDsu
  la <- leaderST dsu a
  sizeLa <- VGM.read parentOrSizeDsu la
  pure (-sizeLa)

{-# INLINEABLE groupsST #-}
groupsST :: Dsu s -> ST s (V.Vector (VU.Vector Int))
groupsST dsu@Dsu {..} = do
  groupSize <- VUM.replicate nDsu (0 :: Int)
  leaders <- VU.generateM nDsu $ \i -> do
    li <- leaderST dsu i
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
