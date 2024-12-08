{-# LANGUAGE TypeFamilies #-}

-- | `AtCoder.LazySegTree.SegAct` instance of range set action over ideomponent monoids. It can set
-- an interval \([l, r)\) to an idempotent monoid \(x\) such as @Max Int@.
module AtCoder.Extra.Monoid.RangeSetId
  ( RangeSetId (..),
  )
where

import AtCoder.LazySegTree (SegAct (..))
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM

-- | `AtCoder.LazySegTree.SegAct` instance of range set action over ideomponent monoid.
newtype RangeSetId a = RangeSetId a
  deriving newtype (Eq, Ord, Show)

instance Semigroup (RangeSetId a) where
  {-# INLINE (<>) #-}
  new <> _ = new

instance (Monoid a) => Monoid (RangeSetId a) where
  {-# INLINE mempty #-}
  mempty = RangeSetId mempty
  {-# INLINE mconcat #-}
  mconcat [] = mempty
  mconcat (a:_) = a

instance (Monoid a) => SegAct (RangeSetId a) a where
  {-# INLINE segAct #-}
  segAct (RangeSetId x) _ = x

newtype instance VU.MVector s (RangeSetId a) = MV_RangeSetId (VU.MVector s a)

newtype instance VU.Vector (RangeSetId a) = V_RangeSetId (VU.Vector a)

deriving instance (VU.Unbox a) => VGM.MVector VUM.MVector (RangeSetId a)

deriving instance (VU.Unbox a) => VG.Vector VU.Vector (RangeSetId a)

instance (VU.Unbox a) => VU.Unbox (RangeSetId a)
