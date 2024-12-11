{-# LANGUAGE TypeFamilies #-}

-- | `AtCoder.LazySegTree.SegAct` instance of range set action over ideomponent monoids. It can set
-- an interval \([l, r)\) to an idempotent monoid \(x\) such as @Max Int@.
--
-- While this monoid is a `SegAct` sample, be warned that it's not guaanteed to be correct.
module AtCoder.Extra.Monoid.RangeSetId
  ( RangeSetId (..),
    new,
    act,
  )
where

import AtCoder.LazySegTree (SegAct (..))
import Data.Semigroup (Max (..), Min (..), stimes)
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM

-- | `AtCoder.LazySegTree.SegAct` instance of range set action over ideomponent monoids.
newtype RangeSetId a = RangeSetId (RangeSetIdRepr a)
  deriving newtype (Eq, Ord, Show)

-- | `RangeSetId` internal representation. The first value represents if it is an identity action.
-- Tuples are not the fastest representation, but it's easier to implement
-- `Data.Vector.Unboxed.Unbox`.
type RangeSetIdRepr a = (Bool, a)

-- | Creates a new `RangeSet` action.
{-# INLINE new #-}
new :: a -> RangeSetId a
new = RangeSetId . (True,)

-- | Acts on @a@.
{-# INLINE act #-}
act :: RangeSetId a -> a -> a
act (RangeSetId (True, !f)) _ = f
act (RangeSetId (False, !_)) x = x

-- segActWithLength works for ideomponent monoids only.

instance Semigroup (RangeSetId a) where
  {-# INLINE (<>) #-}
  RangeSetId (False, !_) <> old = old
  new_ <> _ = new_
  {-# INLINE stimes #-}
  stimes _ x = x

-- The `Monoid` constraint is just for their default value.
instance (Monoid a) => Monoid (RangeSetId a) where
  {-# INLINE mempty #-}
  mempty = RangeSetId (False, mempty)
  {-# INLINE mconcat #-}
  -- find the first non-mempty
  mconcat [] = mempty
  mconcat (RangeSetId (False, !_) : as) = mconcat as
  mconcat (a : _) = a

-- The target is limited to ideomponent monoids. The `Monoid` constraint is just for their default
-- value.
instance (Ord a, Bounded a) => SegAct (RangeSetId (Max a)) (Max a) where
  {-# INLINE segAct #-}
  segAct = act

-- The target is limited to ideomponent monoids. The `Monoid` constraint is just for their default
-- value.
instance (Ord a, Bounded a) => SegAct (RangeSetId (Min a)) (Min a) where
  {-# INLINE segAct #-}
  segAct = act

newtype instance VU.MVector s (RangeSetId a) = MV_RangeSetId (VU.MVector s (RangeSetIdRepr a))

newtype instance VU.Vector (RangeSetId a) = V_RangeSetId (VU.Vector (RangeSetIdRepr a))

deriving instance (VU.Unbox a) => VGM.MVector VUM.MVector (RangeSetId a)

deriving instance (VU.Unbox a) => VG.Vector VU.Vector (RangeSetId a)

instance (VU.Unbox a) => VU.Unbox (RangeSetId a)
