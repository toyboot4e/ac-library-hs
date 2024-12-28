{-# LANGUAGE TypeFamilies #-}

-- | `AtCoder.LazySegTree.SegAct` instance of range set action over ideomponent monoids. It can set
-- an interval \([l, r)\) to an idempotent monoid \(x\) such as @Max Int@.
--
-- @since 1.0.0.0
module AtCoder.Extra.Monoid.RangeSetId
  ( -- * RangeSetId
    RangeSetId (..),

    -- * Constructor
    new,

    -- * Action
    act,
  )
where

import AtCoder.LazySegTree (SegAct (..))
import Data.Bit (Bit (..))
import Data.Semigroup (Max (..), Min (..), stimes)
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM

-- | `AtCoder.LazySegTree.SegAct` instance of range set action over ideomponent monoids.
--
-- ==== Example
-- >>> import AtCoder.Extra.Monoid (SegAct(..), RangeSetId(..))
-- >>> import AtCoder.LazySegTree qualified as LST
-- >>> import Data.Bit (Bit (..))
-- >>> import Data.Semigroup (Max(..))
-- >>> seg <- LST.build @_ @(RangeSetId (Max Int)) @(Max Int) $ VU.generate 3 (Max . (+ 10)) -- [10, 11, 12]
-- >>> LST.applyIn seg 0 2 $ RangeSetId (Bit True, Max 5) -- [5, 5, 12]
-- >>> getMax <$> LST.prod seg 0 3
-- 12
--
-- @since 1.0.0.0
newtype RangeSetId a = RangeSetId (RangeSetIdRepr a)
  deriving newtype
    ( -- | @since 1.0.0.0
      Eq,
      -- | @since 1.0.0.0
      Ord,
      -- | @since 1.0.0.0
      Show
    )

-- | `RangeSetId` internal representation. The first value represents if it is an identity action.
-- Tuples are not the fastest representation, but it's easier to implement
-- `Data.Vector.Unboxed.Unbox`.
--
-- @since 1.0.0.0
type RangeSetIdRepr a = (Bit, a)

-- | Creates a new `RangeSet` action.
--
-- @since 1.0.0.0
{-# INLINE new #-}
new :: a -> RangeSetId a
new = RangeSetId . (Bit True,)

-- | Applies one-length range set: \(f: x \rightarrow y\).
--
-- @since 1.0.0.0
{-# INLINE act #-}
act :: RangeSetId a -> a -> a
act (RangeSetId (Bit True, !f)) _ = f
act (RangeSetId (Bit False, !_)) x = x

-- segActWithLength works for ideomponent monoids only.

-- | @since 1.0.0.0
instance Semigroup (RangeSetId a) where
  {-# INLINE (<>) #-}
  RangeSetId (Bit False, !_) <> old = old
  new_ <> _ = new_
  {-# INLINE stimes #-}
  stimes _ x = x

-- The `Monoid` constraint is just for their default value.

-- | @since 1.0.0.0
instance (Monoid a) => Monoid (RangeSetId a) where
  {-# INLINE mempty #-}
  mempty = RangeSetId (Bit False, mempty)
  {-# INLINE mconcat #-}
  -- find the first non-mempty
  mconcat [] = mempty
  mconcat (RangeSetId (Bit False, !_) : as) = mconcat as
  mconcat (a : _) = a

-- The target is limited to ideomponent monoids. The `Monoid` constraint is just for their default
-- value.

-- | @since 1.0.0.0
instance (Ord a, Bounded a) => SegAct (RangeSetId (Max a)) (Max a) where
  {-# INLINE segAct #-}
  segAct = act

-- The target is limited to ideomponent monoids. The `Monoid` constraint is just for their default
-- value.

-- | @since 1.0.0.0
instance (Ord a, Bounded a) => SegAct (RangeSetId (Min a)) (Min a) where
  {-# INLINE segAct #-}
  segAct = act

-- | @since 1.0.0.0
newtype instance VU.MVector s (RangeSetId a) = MV_RangeSetId (VU.MVector s (RangeSetIdRepr a))

-- | @since 1.0.0.0
newtype instance VU.Vector (RangeSetId a) = V_RangeSetId (VU.Vector (RangeSetIdRepr a))

-- | @since 1.0.0.0
deriving instance (VU.Unbox a) => VGM.MVector VUM.MVector (RangeSetId a)

-- | @since 1.0.0.0
deriving instance (VU.Unbox a) => VG.Vector VU.Vector (RangeSetId a)

-- | @since 1.0.0.0
instance (VU.Unbox a) => VU.Unbox (RangeSetId a)
