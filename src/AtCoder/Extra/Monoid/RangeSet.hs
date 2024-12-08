{-# LANGUAGE TypeFamilies #-}

-- | `AtCoder.LazySegTree.SegAct` instance of range set action. It can set an interval \([l, r)\) to
-- the same monoid \(x\) such as @Sum Int@.
module AtCoder.Extra.Monoid.RangeSet
  ( RangeSet (..),
  )
where

import AtCoder.Extra.Math qualified as ACEM
import AtCoder.LazySegTree (SegAct (..))
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM

-- | `AtCoder.LazySegTree.SegAct` instance of range set action.
newtype RangeSet a = RangeSet a
  deriving newtype (Eq, Ord, Show)

instance Semigroup (RangeSet a) where
  {-# INLINE (<>) #-}
  new <> _ = new

instance (Monoid a) => Monoid (RangeSet a) where
  {-# INLINE mempty #-}
  mempty = RangeSet mempty
  {-# INLINE mconcat #-}
  mconcat [] = RangeSet mempty
  mconcat (a : _) = a

instance (Monoid a) => SegAct (RangeSet a) a where
  {-# INLINE segActWithLength #-}
  segActWithLength !len (RangeSet a) _ = ACEM.power len (<>) a

newtype instance VU.MVector s (RangeSet a) = MV_RangeSet (VU.MVector s a)

newtype instance VU.Vector (RangeSet a) = V_RangeSet (VU.Vector a)

deriving instance (VU.Unbox a) => VGM.MVector VUM.MVector (RangeSet a)

deriving instance (VU.Unbox a) => VG.Vector VU.Vector (RangeSet a)

instance (VU.Unbox a) => VU.Unbox (RangeSet a)
