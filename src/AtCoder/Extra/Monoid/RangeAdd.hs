{-# LANGUAGE TypeFamilies #-}

-- | Monoid action \(f: x \rightarrow x + d\).
--
-- @since 1.0.0.0
module AtCoder.Extra.Monoid.RangeAdd
  ( -- * RangeAdd
    RangeAdd (..),

    -- * Constructor
    new,
    unRangeAdd,

    -- * Actions
    act,
  )
where

import AtCoder.LazySegTree (SegAct (..))
import Data.Semigroup (Max (..), Min (..), Sum (..), stimes)
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM

-- | Monoid action \(f: x \rightarrow x + d\).
--
-- ==== __Example (action on @Sum@)__
-- >>> import AtCoder.Extra.Monoid (SegAct(..), RangeAdd(..))
-- >>> import AtCoder.LazySegTree qualified as LST
-- >>> import Data.Semigroup (Sum(..))
-- >>> seg <- LST.build @_ @(RangeAdd (Sum Int)) @(Sum Int) $ VU.generate 3 Sum -- [0, 1, 2]
-- >>> LST.applyIn seg 0 3 $ RangeAdd (Sum 5) -- [5, 6, 7]
-- >>> getSum <$> LST.prod seg 0 3
-- 18
--
-- ==== __Example (action on @Max@)__
-- >>> import AtCoder.Extra.Monoid (SegAct(..), RangeAdd(..))
-- >>> import AtCoder.LazySegTree qualified as LST
-- >>> import Data.Semigroup (Max(..))
-- >>> seg <- LST.build @_ @(RangeAdd (Max Int)) @(Max Int) $ VU.generate 3 Max -- [0, 1, 2]
-- >>> LST.applyIn seg 0 3 $ RangeAdd (Max 5) -- [5, 6, 7]
-- >>> getMax <$> LST.prod seg 0 3
-- 7
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

-- | \(O(1)\) Retrieves the internal value of `RangeAdd`.
--
-- @since 1.1.0.0
{-# INLINE unRangeAdd #-}
unRangeAdd :: RangeAdd a -> a
unRangeAdd (RangeAdd a) = a

-- | \(O(1)\) Applies one-length range add: \(f: x \rightarrow d + x\).
--
-- @since 1.0.0.0
{-# INLINE act #-}
act :: (Semigroup a) => RangeAdd a -> a -> a
act (RangeAdd dx) x = dx <> x

-- | \(O(1)\) Acts on @a@ with length in terms of `SegAct`. Be warned that it doesn't work well with
-- idempotent monoids such as `Max` or `Min`.
--
-- @since 1.0.0.0
{-# INLINE actWithLength #-}
actWithLength :: (Semigroup a) => Int -> RangeAdd a -> a -> a
actWithLength len (RangeAdd f) x = stimes len f <> x

-- | @since 1.2.0.0
instance (Semigroup a, Num a) => Semigroup (RangeAdd a) where
  {-# INLINE (<>) #-}
  (RangeAdd a) <> (RangeAdd b) = RangeAdd $! a + b

-- | @since 1.2.0.0
instance (Num a, Semigroup a) => Monoid (RangeAdd a) where
  {-# INLINE mempty #-}
  mempty = RangeAdd 0

-- | @since 1.2.0.0
instance (Num a) => SegAct (RangeAdd (Sum a)) (Sum a) where
  {-# INLINE segActWithLength #-}
  segActWithLength = actWithLength

-- | @since 1.1.0.0
instance (Num a, Monoid (Max a)) => SegAct (RangeAdd (Max a)) (Max a) where
  {-# INLINE segAct #-}
  segAct (RangeAdd (Max dx)) (Max x) = Max $! dx + x

-- | @since 1.1.0.0
instance (Num a, Monoid (Min a)) => SegAct (RangeAdd (Min a)) (Min a) where
  {-# INLINE segAct #-}
  segAct (RangeAdd (Min dx)) (Min x) = Min $! dx + x

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
