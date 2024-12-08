{-# LANGUAGE TypeFamilies #-}

-- | Monoid action for setting interval \([l, r)\) to the same value \(x\).
module AtCoder.Extra.Monoid.RangeAdd
  ( RangeAdd (..),
  )
where

import AtCoder.LazySegTree (SegAct (..))
import Data.Foldable (foldl')
import Data.Monoid
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM

-- | Range set monoid action.
newtype RangeAdd a = RangeAdd a
  deriving newtype (Eq, Ord, Show)

instance Semigroup (RangeAdd a) where
  {-# INLINE (<>) #-}
  new <> _ = new

instance (Num a) => Monoid (RangeAdd a) where
  {-# INLINE mempty #-}
  mempty = 0

instance (Num a) => SegAct (RangeAdd a) a where
  {-# INLINE segActWithLength #-}
  segActWithLength !len (RangeAdd a) x = id $! x + a * fromIntegral len

instance (Num a) => SegAct (RangeAdd a) (Sum a) where
  {-# INLINE segActWithLength #-}
  segActWithLength !len (RangeAdd a) (Sum x) = Sum $! x + a * fromIntegral len

instance (Num a) => SegAct (RangeAdd a) (Product a) where
  {-# INLINE segActWithLength #-}
  segActWithLength !len (RangeAdd a) (Product x) = Product $! x + a * fromIntegral len

instance (Num a) => SegAct (RangeAdd a) (Min a) where
  {-# INLINE segActWithLength #-}
  segActWithLength !len (RangeAdd a) (Min x) = Min $! x + a * fromIntegral len

instance (Num a) => SegAct (RangeAdd a) (Max a) where
  {-# INLINE segActWithLength #-}
  segActWithLength !len (RangeAdd a) (Max x) = Max $! power len (<>) a

newtype instance VU.MVector s (RangeAdd a) = MV_RangeAdd (VU.MVector s a)

newtype instance VU.Vector (RangeAdd a) = V_RangeAdd (VU.Vector a)

deriving instance (VU.Unbox a) => VGM.MVector VUM.MVector (RangeAdd a)

deriving instance (VU.Unbox a) => VG.Vector VU.Vector (RangeAdd a)

instance (VU.Unbox a) => VU.Unbox (RangeAdd a)



