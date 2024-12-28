{-# LANGUAGE TypeFamilies #-}

-- | Monoid action for setting interval \([l, r)\).
--
-- @since 1.0.0.0
module AtCoder.Extra.Monoid.RangeAdd
  ( -- * RangeAdd
    RangeAdd (..),

    -- * Constructor
    new,

    -- * Action
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
-- ==== Example
-- >>> import AtCoder.Extra.Monoid (SegAct(..), RangeAdd(..))
-- >>> import AtCoder.LazySegTree qualified as LST
-- >>> import Data.Semigroup (Max(..))
-- >>> seg <- LST.build @_ @(RangeAdd Int) @(Sum Int) $ VU.generate 3 Sum -- [0, 1, 2]
-- >>> LST.applyIn seg 0 3 $ RangeAdd 5 -- [5, 6, 7]
-- >>> getSum <$> LST.prod seg 0 3
-- 18
--
-- @since 1.0.0.0
newtype RangeAdd a = RangeAdd a
  deriving newtype
    ( -- | @since 1.0.0.0
      Eq,
      -- | @since 1.0.0.0
      Ord,
      -- | @since 1.0.0.0
      Show
    )

-- | Creates `RangeAdd`.
--
-- @since 1.0.0.0
{-# INLINE new #-}
new :: a -> RangeAdd a
new = RangeAdd

-- | Applies one-length range add: \(f: x \rightarrow d + x\).
--
-- @since 1.0.0.0
{-# INLINE act #-}
act :: (Num a) => RangeAdd a -> a -> a
act (RangeAdd dx) x = dx + x

-- | Acts on @a@ with length in terms of `SegAct`.
--
-- @since 1.0.0.0
{-# INLINE actWithLength #-}
actWithLength :: (Num a) => Int -> RangeAdd a -> a -> a
actWithLength len (RangeAdd f) x = fromIntegral len * f + x

-- | @since 1.0.0.0
instance (Num a) => Semigroup (RangeAdd a) where
  {-# INLINE (<>) #-}
  (RangeAdd a) <> (RangeAdd b) = RangeAdd $! a + b

-- | @since 1.0.0.0
instance (Num a) => Monoid (RangeAdd a) where
  {-# INLINE mempty #-}
  mempty = RangeAdd 0

-- | @since 1.0.0.0
instance (Num a) => SegAct (RangeAdd a) (Sum a) where
  {-# INLINE segActWithLength #-}
  segActWithLength len a (Sum x) = Sum $! actWithLength len a x

-- not works as SegAct for Product, Min, and Max.

-- | @since 1.0.0.0
newtype instance VU.MVector s (RangeAdd a) = MV_RangeAdd (VU.MVector s a)

-- | @since 1.0.0.0
newtype instance VU.Vector (RangeAdd a) = V_RangeAdd (VU.Vector a)

-- | @since 1.0.0.0
deriving instance (VU.Unbox a) => VGM.MVector VUM.MVector (RangeAdd a)

-- | @since 1.0.0.0
deriving instance (VU.Unbox a) => VG.Vector VU.Vector (RangeAdd a)

-- | @since 1.0.0.0
instance (VU.Unbox a) => VU.Unbox (RangeAdd a)
