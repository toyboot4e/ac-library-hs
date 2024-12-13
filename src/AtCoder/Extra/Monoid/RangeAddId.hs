{-# LANGUAGE TypeFamilies #-}

-- | Monoid action for setting interval \([l, r)\) over ideomponent monoids.
module AtCoder.Extra.Monoid.RangeAddId
  ( RangeAddId (..),
    new,
    act,
  )
where

import AtCoder.LazySegTree (SegAct (..))
import Data.Semigroup (Max (..), Min (..))
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM

-- | Range set monoid action.
--
-- = Example
-- >>> import AtCoder.Extra.Monoid (SegAct(..), RangeAddId(..))
-- >>> import AtCoder.LazySegTree qualified as LST
-- >>> import Data.Semigroup (Max(..))
-- >>> seg <- LST.build @_ @(RangeAddId Int) @(Max Int) $ VU.generate 3 Max -- [0, 1, 2]
-- >>> LST.applyIn seg 0 3 $ RangeAddId 5 -- [5, 6, 7]
-- >>> getMax <$> LST.prod seg 0 3
-- 7
newtype RangeAddId a = RangeAddId a
  deriving newtype (Eq, Ord, Show)

-- | Creates `RangeAddId`.
{-# INLINE new #-}
new :: a -> RangeAddId a
new = RangeAddId

-- | Acts on @a@.
{-# INLINE act #-}
act :: (Num a) => RangeAddId a -> a -> a
act (RangeAddId f) x = f + x

instance (Num a) => Semigroup (RangeAddId a) where
  {-# INLINE (<>) #-}
  (RangeAddId a) <> (RangeAddId b) = RangeAddId $! a + b

instance (Num a) => Monoid (RangeAddId a) where
  {-# INLINE mempty #-}
  mempty = RangeAddId 0

instance (Num a) => SegAct (RangeAddId a) (Max a) where
  {-# INLINE segAct #-}
  segAct f (Max x) = Max $! act f x

instance (Num a) => SegAct (RangeAddId a) (Min a) where
  {-# INLINE segAct #-}
  segAct f (Min x) = Min $! act f x

-- not works as SegAct for Sum and Product.

newtype instance VU.MVector s (RangeAddId a) = MV_RangeAddId (VU.MVector s a)

newtype instance VU.Vector (RangeAddId a) = V_RangeAddId (VU.Vector a)

deriving instance (VU.Unbox a) => VGM.MVector VUM.MVector (RangeAddId a)

deriving instance (VU.Unbox a) => VG.Vector VU.Vector (RangeAddId a)

instance (VU.Unbox a) => VU.Unbox (RangeAddId a)
