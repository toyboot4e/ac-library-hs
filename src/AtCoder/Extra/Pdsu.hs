{-# LANGUAGE RecordWildCards #-}

-- original implementation:
-- <https://qiita.com/drken/items/cce6fc5c579051e64fab>

-- | A potentialized disjoint set union on a [group](https://en.wikipedia.org/wiki/Group_(mathematics\))
-- under a differential constraint system. Each vertex \(v\) is assigned a potential value \(p(v)\),
-- where representatives (`leader`) of each group have a potential value of `mempty`, and other
-- vertices have potentials relative to their representative.
--
-- The group type is represented as a `Monoid` with a inverse operator, passed on `new`. This
-- approach avoids defining a separate typeclass for groups.
--
-- ==== Invariant
-- New monoids always come from the left: @new <> old@. The order is important for non-commutative
-- monoid implementations.
--
-- @since 1.1.0.0
module AtCoder.Extra.Pdsu
  ( -- * Pdsu
    Pdsu (nPdsu),

    -- * Constructors
    new,

    -- * Inspection
    leader,
    pot,
    diff,
    unsafeDiff,
    same,
    canMerge,

    -- * Merging
    merge,
    merge_,

    -- * Group information
    size,
    groups,

    -- * Reset
    clear,
  )
where

import AtCoder.Internal.Assert qualified as ACIA
import Control.Monad
import Control.Monad.Primitive (PrimMonad, PrimState, stToPrim)
import Control.Monad.ST (ST)
import Data.Vector qualified as V
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Stack (HasCallStack)

-- | A potentialized disjoint set union on a [group](https://en.wikipedia.org/wiki/Group_(mathematics\))
-- under a differential constraint system. Each vertex \(v\) is assigned a potential value \(p(v)\),
--
-- ==== __Example__
-- Create a `Pdsu` with four vertices with potential type @Sum Int@. Use `negate` as the inverse
-- operator:
--
-- >>> import AtCoder.Extra.Pdsu qualified as Pdsu
-- >>> import Data.Semigroup (Sum (..))
-- >>> dsu <- Pdsu.new @_ @(Sum Int) 4 negate
--
-- The API is similar to @Dsu@, but with differential potential values:
--
-- >>> Pdsu.merge dsu 1 0 (Sum 1)  -- p(1) - p(0) := Sum 1
-- True
--
-- >>> Pdsu.merge_ dsu 2 0 (Sum 2) -- p(2) - p(0) := Sum 2
-- >>> Pdsu.leader dsu 0
-- 0
--
-- Potential values can be retrieved with `pot`:
--
-- >>> Pdsu.pot dsu 0
-- Sum {getSum = 0}
--
-- >>> Pdsu.pot dsu 1
-- Sum {getSum = 1}
--
-- >>> Pdsu.pot dsu 2
-- Sum {getSum = 2}
--
-- Difference of potentials in the same group can be retrieved with `diff`:
--
-- >>> Pdsu.diff dsu 2 1
-- Just (Sum {getSum = 1})
--
-- >>> Pdsu.diff dsu 2 3
-- Nothing
--
-- Retrieve group information with `groups`
--
-- >>> Pdsu.groups dsu
-- [[2,1,0],[3]]
--
-- @since 1.1.0.0
data Pdsu s a = Pdsu
  { -- | The number of vertices.
    nPdsu :: {-# UNPACK #-} !Int,
    -- | Parent: non-positive, size: positive
    parentOrSizePdsu :: !(VUM.MVector s Int),
    -- | Diffierencial potential of each vertex.
    potentialPdsu :: !(VUM.MVector s a),
    invertPdsu :: !(a -> a)
  }

-- | \(O(n)\) Creates a new DSU under a differential constraint system.
--
-- @since 1.1.0.0
{-# INLINE new #-}
new ::
  forall m a.
  (PrimMonad m, Monoid a, VU.Unbox a) =>
  -- | The number of vertices.
  Int ->
  -- | The inverse operator of the monoid.
  (a -> a) ->
  -- | A potencialized DSU.
  m (Pdsu (PrimState m) a)
new n f = Pdsu n <$> VUM.replicate n (-1 {- size 1 -}) <*> VUM.replicate n (mempty :: a) <*> pure f

-- | \(O(\alpha(n))\) Returns the representative of the connected component that contains the
-- vertex.
--
-- @since 1.1.0.0
{-# INLINE leader #-}
leader :: (HasCallStack, PrimMonad m, Semigroup a, VU.Unbox a) => Pdsu (PrimState m) a -> Int -> m Int
leader pdsu v0 = stToPrim $ leaderST pdsu v0
  where
    !_ = ACIA.checkIndex "AtCoder.Extra.Pdsu.leader" v0 $ nPdsu pdsu

-- | \(O(\alpha(n))\) Returns \(p(v)\), the potential value of vertex \(v\) relative to the
-- reprensetative of its group.
--
-- @since 1.1.0.0
{-# INLINE pot #-}
pot :: (HasCallStack, PrimMonad m, Semigroup a, VU.Unbox a) => Pdsu (PrimState m) a -> Int -> m a
pot dsu v1 = stToPrim $ potST dsu v1

-- | \(O(\alpha(n))\) Returns whether the vertices \(a\) and \(b\) are in the same connected
-- component.
--
-- @since 1.1.0.0
{-# INLINE same #-}
same :: (HasCallStack, PrimMonad m, Semigroup a, VU.Unbox a) => Pdsu (PrimState m) a -> Int -> Int -> m Bool
same dsu v1 v2 = stToPrim $ sameST dsu v1 v2

-- | \(O(\alpha(n))\) Returns the potential of \(v_1\) relative to \(v_2\): \(p(v_1) \cdot p^{-1}(v_2)\)
-- if the two vertices belong to the same group. Returns `Nothing` when the two vertices are not
-- connected.
--
-- @since 1.1.0.0
{-# INLINE diff #-}
diff :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => Pdsu (PrimState m) a -> Int -> Int -> m (Maybe a)
diff dsu v1 v2 = stToPrim $ diffST dsu v1 v2

-- | \(O(\alpha(n))\) Returns the potential of \(v_1\) relative to \(v_2\): \(p(v_1) \cdot p^{-1}(v_2)\)
-- if the two vertices belong to the same group. Returns meaningless value if the two vertices are
-- not connected.
--
-- @since 1.1.0.0
{-# INLINE unsafeDiff #-}
unsafeDiff :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => Pdsu (PrimState m) a -> Int -> Int -> m a
unsafeDiff dsu v1 v2 = stToPrim $ unsafeDiffST dsu v1 v2

-- TODO: use merge and mergeMaybe

-- | \(O(\alpha(n))\) Merges \(v_1\) to \(v_2\) with differential (relative) potential
-- \(\mathrm{dp}\): \(p(v1) := \mathrm{dp} \cdot p(v2)\). Returns `True` if they're newly merged.
--
-- @since 1.1.0.0
{-# INLINE merge #-}
merge :: (HasCallStack, PrimMonad m, Monoid a, Ord a, VU.Unbox a) => Pdsu (PrimState m) a -> Int -> Int -> a -> m Bool
merge dsu v10 v20 !dp0 = stToPrim $ mergeST dsu v10 v20 dp0
  where
    !_ = ACIA.checkIndex "AtCoder.Extra.Pdsu.merge" v10 $ nPdsu dsu
    !_ = ACIA.checkIndex "AtCoder.Extra.Pdsu.merge" v20 $ nPdsu dsu

-- | \(O(\alpha(n))\) `merge` with the return value discarded.
--
-- @since 1.1.0.0
{-# INLINE merge_ #-}
merge_ :: (HasCallStack, PrimMonad m, Monoid a, Ord a, VU.Unbox a) => Pdsu (PrimState m) a -> Int -> Int -> a -> m ()
merge_ !dsu !v1 !v2 !dp = stToPrim $ do
  _ <- mergeST dsu v1 v2 dp
  pure ()

-- | \(O(\alpha(n))\) Returns `True` if the two vertices belong to different groups or they belong
-- to the same group under the condition \(p(v_1) = dp \cdot p(v_2)\). It's just a convenient
-- helper function.
--
-- @since 1.1.0.0
{-# INLINE canMerge #-}
canMerge :: (HasCallStack, PrimMonad m, Semigroup a, Eq a, VU.Unbox a) => Pdsu (PrimState m) a -> Int -> Int -> a -> m Bool
canMerge dsu v1 v2 dp = stToPrim $ canMergeST dsu v1 v2 dp

-- | \(O(\alpha(n))\) Returns the number of vertices belonging to the same group.
--
-- @since 1.1.0.0
{-# INLINE size #-}
size :: (HasCallStack, PrimMonad m, Semigroup a, VU.Unbox a) => Pdsu (PrimState m) a -> Int -> m Int
size !dsu !v = stToPrim $ do
  l <- leaderST dsu v
  negate <$> VGM.read (parentOrSizePdsu dsu) l
  where
    !_ = ACIA.checkIndex "AtCoder.Extra.Pdsu.size" v $ nPdsu dsu

-- | \(O(n)\) Divides the graph into connected components and returns the list of them.
--
-- @since 1.1.0.0
{-# INLINE groups #-}
groups :: (PrimMonad m, Semigroup a, VU.Unbox a) => Pdsu (PrimState m) a -> m (V.Vector (VU.Vector Int))
groups dsu = stToPrim $ groupsST dsu

-- | \(O(n)\) Clears the `Pdsu` to the initial state.
--
-- @since 1.1.0.0
{-# INLINE clear #-}
clear :: forall m a. (PrimMonad m, Monoid a, VU.Unbox a) => Pdsu (PrimState m) a -> m ()
clear !dsu = do
  VGM.set (potentialPdsu dsu) (mempty @a)
  VGM.set (parentOrSizePdsu dsu) (-1 {- size -})

-- -------------------------------------------------------------------------------------------------
-- Internal
-- -------------------------------------------------------------------------------------------------

{-# INLINE leaderST #-}
-- NOTE(perf): INLINE makes it a bit faster
leaderST :: (Semigroup a, VU.Unbox a) => Pdsu s a -> Int -> ST s Int
leaderST Pdsu {..} v0 = inner v0
  where
    inner v = do
      p <- VGM.read parentOrSizePdsu v
      if {- size? -} p < 0
        then pure v
        else do
          -- NOTE(perf): Path compression.
          -- Handle the nodes closer to the root first and move them onto just under the root
          !r <- inner p
          when (p /= r) $ do
            !pp <- VGM.read potentialPdsu p
            -- Move `v` to just under the root:
            VGM.write parentOrSizePdsu v {- root -} r
            -- INVARIANT: new coming monoids always come from the left. And we're performing
            -- reverse folding.
            VGM.modify potentialPdsu (<> pp) v
          pure r

{-# INLINEABLE potST #-}
potST :: (HasCallStack, Semigroup a, VU.Unbox a) => Pdsu s a -> Int -> ST s a
potST dsu@Pdsu {..} v1 = do
  -- Perform path compression
  _ <- leaderST dsu v1
  VGM.read potentialPdsu v1
  where
    !_ = ACIA.checkIndex "AtCoder.Extra.Pdsu.potST" v1 nPdsu

{-# INLINEABLE mergeST #-}
mergeST :: (HasCallStack, Monoid a, Ord a, VU.Unbox a) => Pdsu s a -> Int -> Int -> a -> ST s Bool
mergeST dsu@Pdsu {..} v10 v20 !dp0 = inner v10 v20 dp0
  where
    inner v1 v2 !dp = do
      !r1 <- leaderST dsu v1
      !r2 <- leaderST dsu v2
      if r1 == r2
        then pure False
        else do
          -- NOTE(perf): Union by size (choose smaller one for root).
          -- Another, more proper optimization would be union by rank (depth).
          !size1 <- VGM.read potentialPdsu v1
          !size2 <- VGM.read potentialPdsu v2
          if size1 >= size2
            then do
              -- Merge `r1` onto `r2`

              -- Update the size of `r1`
              !negativeSize1 <- negate {- retrieve size -} <$> VGM.read parentOrSizePdsu r1
              !negativeSize2 <- negate {- retrieve size -} <$> VGM.read parentOrSizePdsu r2
              VGM.write parentOrSizePdsu r1 ({- size -} negativeSize1 + negativeSize2)

              -- p(v1) becomes p'(v1) under r2 after merge. p(r1) becomes p'(r1).
              --     p'(v1) = dp <> p(v2)
              --     p'(v1) = p(v1) <> 'p(r1)
              -- Therefore,
              --     p'(r1) = p^{-1}(v1) <> dp <> p(v2)
              !p1 <- VGM.read potentialPdsu v1
              !p2 <- VGM.read potentialPdsu v2
              let !pr1' = invertPdsu p1 <> dp <> p2

              -- Move `r1` to just under `r2`:
              VGM.write parentOrSizePdsu r1 {- record new root -} r2
              VGM.write potentialPdsu r1 pr1'

              pure True
            else do
              inner v2 v1 $ invertPdsu dp

{-# INLINEABLE canMergeST #-}
canMergeST :: (HasCallStack, Semigroup a, Eq a, VU.Unbox a) => Pdsu s a -> Int -> Int -> a -> ST s Bool
canMergeST dsu v1 v2 dp = do
  b <- sameST dsu v1 v2
  if not b
    then pure True
    else do
      !p1 <- VGM.read (potentialPdsu dsu) v1
      !p2 <- VGM.read (potentialPdsu dsu) v2
      pure $ p1 == dp <> p2

{-# INLINEABLE sameST #-}
sameST :: (HasCallStack, Semigroup a, VU.Unbox a) => Pdsu s a -> Int -> Int -> ST s Bool
sameST !dsu !v1 !v2 = stToPrim $ do
  l1 <- leaderST dsu v1
  l2 <- leaderST dsu v2
  pure $ l1 == l2
  where
    !_ = ACIA.checkIndex "AtCoder.Extra.Pdsu.sameST" v1 $ nPdsu dsu
    !_ = ACIA.checkIndex "AtCoder.Extra.Pdsu.sameST" v2 $ nPdsu dsu

{-# INLINEABLE diffST #-}
diffST :: (HasCallStack, Monoid a, VU.Unbox a) => Pdsu s a -> Int -> Int -> ST s (Maybe a)
diffST dsu v1 v2 = do
  b <- sameST dsu v1 v2
  if b
    then Just <$> unsafeDiffST dsu v1 v2
    else pure Nothing

{-# INLINEABLE unsafeDiffST #-}
unsafeDiffST :: (HasCallStack, Monoid a, VU.Unbox a) => Pdsu s a -> Int -> Int -> ST s a
unsafeDiffST !dsu !v1 !v2 = do
  p1 <- potST dsu v1
  p2 <- potST dsu v2
  pure $ p1 <> invertPdsu dsu p2

{-# INLINEABLE groupsST #-}
groupsST :: (Semigroup a, VU.Unbox a) => Pdsu s a -> ST s (V.Vector (VU.Vector Int))
groupsST dsu@Pdsu {..} = do
  groupSize <- VUM.replicate nPdsu (0 :: Int)
  leaders <- VU.generateM nPdsu $ \i -> do
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
