{-# LANGUAGE TypeFamilies #-}

-- | Monoid action for setting interval \([l, r)\).
module AtCoder.Extra.Monoid.RangeAdd
  ( RangeAdd (..),
    new,
    act,
  )
where

import AtCoder.LazySegTree (SegAct (..))
import Data.Semigroup (Sum (..))
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM

-- | Range set monoid action.
--
-- = Example
-- >>> import AtCoder.Extra.Monoid (SegAct(..), RangeAdd(..))
-- >>> import AtCoder.LazySegTree qualified as LST
-- >>> import Data.Semigroup (Max(..))
-- >>> seg <- LST.build @_ @(RangeAdd Int) @(Sum Int) $ VU.generate 3 Sum -- [0, 1, 2]
-- >>> LST.applyIn seg 0 3 $ RangeAdd 5 -- [5, 6, 7]
-- >>> getSum <$> LST.prod seg 0 3
-- 18
newtype RangeAdd a = RangeAdd a
  deriving newtype (Eq, Ord, Show)

-- | Creates `RangeAdd`.
{-# INLINE new #-}
new :: a -> RangeAdd a
new = RangeAdd

-- | Acts on @a@.
{-# INLINE act #-}
act :: (Num a) => RangeAdd a -> a -> a
act (RangeAdd f) x = f + x

-- | Acts on @a@ with length in terms of `SegAct`.
{-# INLINE actWithLength #-}
actWithLength :: (Num a) => Int -> RangeAdd a -> a -> a
actWithLength len (RangeAdd f) x = fromIntegral len * f + x

instance (Num a) => Semigroup (RangeAdd a) where
  {-# INLINE (<>) #-}
  (RangeAdd a) <> (RangeAdd b) = RangeAdd $! a + b

instance (Num a) => Monoid (RangeAdd a) where
  {-# INLINE mempty #-}
  mempty = RangeAdd 0

instance (Num a) => SegAct (RangeAdd a) (Sum a) where
  {-# INLINE segActWithLength #-}
  segActWithLength len a (Sum x) = Sum $! actWithLength len a x

-- not works as SegAct for Product, Min, and Max.

newtype instance VU.MVector s (RangeAdd a) = MV_RangeAdd (VU.MVector s a)

newtype instance VU.Vector (RangeAdd a) = V_RangeAdd (VU.Vector a)

deriving instance (VU.Unbox a) => VGM.MVector VUM.MVector (RangeAdd a)

deriving instance (VU.Unbox a) => VG.Vector VU.Vector (RangeAdd a)

instance (VU.Unbox a) => VU.Unbox (RangeAdd a)
