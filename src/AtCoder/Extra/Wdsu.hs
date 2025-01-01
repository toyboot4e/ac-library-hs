{-# LANGUAGE RecordWildCards #-}

-- original implementation:
-- <https://qiita.com/drken/items/cce6fc5c579051e64fab>

-- | A disjoint set union on a [group](https://en.wikipedia.org/wiki/Group_(mathematics\)) under a
-- differential constraint system. Each vertex \(v\) is assigned a potential value \(p(v)\),
-- where representatives of each group have a potential of `mempty`, and other vertices have
-- potentials relative to their representative.
--
-- The group type is represented as a `Monoid` with a inverse operator, passed on `new`. This
-- approach avoids defining a separate typeclass for groups.
--
-- ==== Invariant
-- New monoids always come from the left: @new <> old@. The order is important for non-commutative
-- monoid implementations.
--
-- @since 1.1.0.0
module AtCoder.Extra.Wdsu
  ( -- * Wdsu
    Wdsu (nWdsu),

    -- * Constructor
    new,

    -- * Vertex information
    leader,
    pot,

    -- * Merging
    merge,
    merge_,

    -- * Two vertex information
    diff,
    unsafeDiff,
    same,
    canMerge,

    -- * Group information
    size,
    groups,

    -- * Clearing
    clear,
  )
where

import AtCoder.Internal.Assert qualified as ACIA
import Control.Monad
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Vector qualified as V
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Stack (HasCallStack)

-- | A disjoint set union on a [group](https://en.wikipedia.org/wiki/Group_(mathematics\)) under a
-- differential constraint system.
--
-- ==== __Example__
-- Create a `Wdsu` with four vertices with potential type @Sum Int@. Use `negate` as the inverse
-- operator:
--
-- >>> import AtCoder.Extra.Wdsu qualified as Wdsu
-- >>> import Data.Semigroup (Sum (..))
-- >>> dsu <- Wdsu.new @_ @(Sum Int) 4 negate
--
-- The API is similar to @Dsu@, but with differential potential values:
--
-- >>> Wdsu.merge dsu 1 0 (Sum 1)  -- p(1) - p(0) := Sum 1
-- True
--
-- >>> Wdsu.merge_ dsu 2 0 (Sum 2) -- p(2) - p(0) := Sum 2
-- >>> Wdsu.leader dsu 0
-- 0
--
-- Potential values can be retrieved with `pot`:
--
-- >>> Wdsu.pot dsu 0
-- Sum {getSum = 0}
--
-- >>> Wdsu.pot dsu 1
-- Sum {getSum = 1}
--
-- >>> Wdsu.pot dsu 2
-- Sum {getSum = 2}
--
-- Difference of potentials in the same group can be retrieved with `diff`:
--
-- >>> Wdsu.diff dsu 2 1
-- Just (Sum {getSum = 1})
--
-- >>> Wdsu.diff dsu 2 3
-- Nothing
--
-- Retrieve group information with `groups`
--
-- >>> Wdsu.groups dsu
-- [[2,1,0],[3]]
--
-- @since 1.1.0.0
data Wdsu s a = Wdsu
  { -- | The number of vertices.
    nWdsu :: {-# UNPACK #-} !Int,
    -- | Parent: non-positive, size: positive
    parentOrSizeWdsu :: !(VUM.MVector s Int),
    -- | Diffierencial potential of each vertex.
    potentialWdsu :: !(VUM.MVector s a),
    invertWdsu :: !(a -> a)
  }

-- | \(O(n)\) Creates a new DSU under a differential constraint system. The second argument is the
-- inverse operator of potential values.
--
-- @since 1.1.0.0
{-# INLINE new #-}
new :: forall m a. (PrimMonad m, Monoid a, VU.Unbox a) => Int -> (a -> a) -> m (Wdsu (PrimState m) a)
new n f = Wdsu n <$> VUM.replicate n (-1 {- size 1 -}) <*> VUM.replicate n (mempty :: a) <*> pure f

-- | \(O(\alpha(n))\) Returns the representative of the connected component that contains the
-- vertex.
--
-- @since 1.1.0.0
{-# INLINE leader #-}
leader :: (HasCallStack, PrimMonad m, Semigroup a, VU.Unbox a) => Wdsu (PrimState m) a -> Int -> m Int
leader Wdsu {..} v0 = inner v0
  where
    !_ = ACIA.checkIndex "AtCoder.Extra.Wdsu.leader" v0 nWdsu
    inner v = do
      p <- VGM.read parentOrSizeWdsu v
      if {- size? -} p < 0
        then pure v
        else do
          -- NOTE(perf): Path compression.
          -- Handle the nodes closer to the root first and move them onto just under the root
          !r <- inner p
          when (p /= r) $ do
            !pp <- VGM.read potentialWdsu p
            -- Move `v` to just under the root:
            VGM.write parentOrSizeWdsu v {- root -} r
            -- INVARIANT: new coming monoids always come from the left. And we're performing
            -- reverse folding.
            VGM.modify potentialWdsu (<> pp) v
          pure r

-- | \(O(\alpha(n))\) Returns \(p(v)\), the potential of \(v\) in their group.
--
-- @since 1.1.0.0
{-# INLINE pot #-}
pot :: (HasCallStack, PrimMonad m, Semigroup a, VU.Unbox a) => Wdsu (PrimState m) a -> Int -> m a
pot dsu@Wdsu {..} v1 = do
  -- Perform path compression
  _ <- leader dsu v1
  VGM.read potentialWdsu v1
  where
    !_ = ACIA.checkIndex "AtCoder.Extra.Wdsu.pot" v1 nWdsu

-- | \(O(\alpha(n))\) Merges \(v_1\) to \(v_2\) with differential (relative) potential \(dp\):
-- \(p(v1) := dp \cdot p(v2)\). Returns `True` if they're newly merged.
--
-- @since 1.1.0.0
{-# INLINE merge #-}
merge :: (HasCallStack, PrimMonad m, Monoid a, Ord a, VU.Unbox a) => Wdsu (PrimState m) a -> Int -> Int -> a -> m Bool
merge dsu@Wdsu {..} v10 v20 !dp0 = inner v10 v20 dp0
  where
    !_ = ACIA.checkIndex "AtCoder.Extra.Wdsu.merge" v10 nWdsu
    !_ = ACIA.checkIndex "AtCoder.Extra.Wdsu.merge" v20 nWdsu
    inner v1 v2 !dp = do
      !r1 <- leader dsu v1
      !r2 <- leader dsu v2
      if r1 == r2
        then pure False
        else do
          -- NOTE(perf): Union by size (choose smaller one for root).
          -- Another, more proper optimization would be union by rank (depth).
          !size1 <- VGM.read potentialWdsu v1
          !size2 <- VGM.read potentialWdsu v2
          if size1 >= size2
            then do
              -- Merge `r1` onto `r2`

              -- Update the size of `r1`
              !negativeSize1 <- negate {- retrieve size -} <$> VGM.read parentOrSizeWdsu r1
              !negativeSize2 <- negate {- retrieve size -} <$> VGM.read parentOrSizeWdsu r2
              VGM.write parentOrSizeWdsu r1 ({- size -} negativeSize1 + negativeSize2)

              -- p(v1) becomes p'(v1) under r2 after merge. p(r1) becomes p'(r1).
              --     p'(v1) = dp <> p(v2)
              --     p'(v1) = p(v1) <> 'p(r1)
              -- Therefore,
              --     p'(r1) = p^{-1}(v1) <> dp <> p(v2)
              !p1 <- VGM.read potentialWdsu v1
              !p2 <- VGM.read potentialWdsu v2
              let !pr1' = invertWdsu p1 <> dp <> p2

              -- Move `r1` to just under `r2`:
              VGM.write parentOrSizeWdsu r1 {- record new root -} r2
              VGM.write potentialWdsu r1 pr1'

              pure True
            else do
              inner v2 v1 $ invertWdsu dp

-- | \(O(\alpha(n))\) `merge` with the return value discarded.
--
-- @since 1.1.0.0
{-# INLINE merge_ #-}
merge_ :: (HasCallStack, PrimMonad m, Monoid a, Ord a, VU.Unbox a) => Wdsu (PrimState m) a -> Int -> Int -> a -> m ()
merge_ !dsu !v1 !v2 !dp = do
  _ <- merge dsu v1 v2 dp
  pure ()

-- | \(O(\alpha(n))\) Returns whether the vertices \(a\) and \(b\) are in the same connected
-- component.
--
-- @since 1.1.0.0
{-# INLINE same #-}
same :: (HasCallStack, PrimMonad m, Semigroup a, VU.Unbox a) => Wdsu (PrimState m) a -> Int -> Int -> m Bool
same !dsu !v1 !v2 = (==) <$> leader dsu v1 <*> leader dsu v2

-- TODO: call it unsafeDiff

-- | \(O(\alpha(n))\) Returns the potential of \(v_1\) relative to \(v_2\): \(p(v_1) \cdot p^{-1}(v_2)\)
-- if the two vertices belong to the same group. Returns `Nothing` when the two vertices are not
-- connected.
--
-- @since 1.1.0.0
{-# INLINE diff #-}
diff :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => Wdsu (PrimState m) a -> Int -> Int -> m (Maybe a)
diff !dsu !v1 !v2 = do
  b <- same dsu v1 v2
  if b
    then Just <$> unsafeDiff dsu v1 v2
    else pure Nothing

-- | \(O(\alpha(n))\) Returns the potential of \(v_1\) relative to \(v_2\): \(p(v_1) \cdot p^{-1}(v_2)\)
-- if the two vertices belong to the same group. Returns meaningless value if the two vertices are
-- not connected.
--
-- @since 1.1.0.0
{-# INLINE unsafeDiff #-}
unsafeDiff :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => Wdsu (PrimState m) a -> Int -> Int -> m a
unsafeDiff !dsu !v1 !v2 = do
  p1 <- pot dsu v1
  p2 <- pot dsu v2
  pure $ p1 <> invertWdsu dsu p2

-- | \(O(\alpha(n))\) Returns `True` if the two vertices belong to different groups or they belong
-- to the same group under the condition \(p(v_1) = dp \cdot p(v_2)\). It's just a convenient
-- helper function.
--
-- @since 1.1.0.0
{-# INLINE canMerge #-}
canMerge :: (HasCallStack, PrimMonad m, Semigroup a, Eq a, VU.Unbox a) => Wdsu (PrimState m) a -> Int -> Int -> a -> m Bool
canMerge !dsu !v1 !v2 !dp = do
  b <- same dsu v1 v2
  if not b
    then pure True
    else do
      !p1 <- VGM.read (potentialWdsu dsu) v1
      !p2 <- VGM.read (potentialWdsu dsu) v2
      pure $ p1 == dp <> p2

-- | \(O(\alpha(n))\) Returns the number of vertices belonging to the same group.
--
-- @since 1.1.0.0
{-# INLINE size #-}
size :: (HasCallStack, PrimMonad m, Semigroup a, VU.Unbox a) => Wdsu (PrimState m) a -> Int -> m Int
size !dsu !v = (negate <$>) . VGM.read (parentOrSizeWdsu dsu) =<< leader dsu v

-- | \(O(n)\) Divides the graph into connected components and returns the list of them.
--
-- @since 1.1.0.0
{-# INLINE groups #-}
groups :: (PrimMonad m, Semigroup a, VU.Unbox a) => Wdsu (PrimState m) a -> m (V.Vector (VU.Vector Int))
groups dsu@Wdsu {..} = do
  groupSize <- VUM.replicate nWdsu (0 :: Int)
  leaders <- VU.generateM nWdsu $ \i -> do
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

-- | \(O(n)\) Clears the `Wdsu` to the initial state.
--
-- @since 1.1.0.0
{-# INLINE clear #-}
clear :: forall m a. (PrimMonad m, Monoid a, VU.Unbox a) => Wdsu (PrimState m) a -> m ()
clear !dsu = do
  VGM.set (potentialWdsu dsu) (mempty @a)
  VGM.set (parentOrSizeWdsu dsu) (-1 {- size -})
