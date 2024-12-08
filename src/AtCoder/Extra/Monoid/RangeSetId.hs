{-# LANGUAGE TypeFamilies #-}

-- | `AtCoder.LazySegTree.SegAct` instance of range set action over ideomponent monoids. It can set
-- an interval \([l, r)\) to an idempotent monoid \(x\) such as @Max Int@.
module AtCoder.Extra.Monoid.RangeSetId
  ( RangeSetId (..),
    new,
  )
where

import AtCoder.LazySegTree (SegAct (..))
import Data.Semigroup (stimes)
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM

-- | `AtCoder.LazySegTree.SegAct` instance of range set action over ideomponent monoid.
newtype RangeSetId a = RangeSetId (RangeSetIdRepr a)
  deriving newtype (Eq, Ord, Show)

-- | `RangeSetId` internal representation. The first value represents if it is an identity action.
-- Tuples are not the fastest representation, but it's easier to implement
-- `Data.Vector.Unboxed.Unbox`.
type RangeSetIdRepr a = (Bool, a)

-- TODO: Monoid requirement on `a` should not be required

-- | Creates a new `RangeSet` action.
new :: a -> RangeSetId a
new = RangeSetId . (True,)

instance Semigroup (RangeSetId a) where
  {-# INLINE (<>) #-}
  RangeSetId (False, !_) <> old = old
  new_ <> _ = new_
  {-# INLINE stimes #-}
  stimes _ x = x

instance (Monoid a) => Monoid (RangeSetId a) where
  {-# INLINE mempty #-}
  mempty = RangeSetId (False, mempty)
  {-# INLINE mconcat #-}
  -- find the first non-mempty
  mconcat [] = mempty
  mconcat (RangeSetId (False, !_) : as) = mconcat as
  mconcat (a : _) = a

instance (Monoid a) => SegAct (RangeSetId a) a where
  {-# INLINE segAct #-}
  segAct (RangeSetId (False, !_)) x = x
  segAct (RangeSetId (True, !a)) _ = a

newtype instance VU.MVector s (RangeSetId a) = MV_RangeSetId (VU.MVector s (RangeSetIdRepr a))

newtype instance VU.Vector (RangeSetId a) = V_RangeSetId (VU.Vector (RangeSetIdRepr a))

deriving instance (VU.Unbox a) => VGM.MVector VUM.MVector (RangeSetId a)

deriving instance (VU.Unbox a) => VG.Vector VU.Vector (RangeSetId a)

instance (VU.Unbox a) => VU.Unbox (RangeSetId a)
