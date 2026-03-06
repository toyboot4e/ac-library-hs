{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

-- | Monoid action \(f: x \rightarrow a\).
--
-- @since 1.6.0.0
module AtCoder.Extra.Monoid.RangeWrite
  ( -- * RangeWrite
    RangeWrite (..),
    RangeWriteRepr,

    -- * Constructors
    new,
    unRangeWrite,

    -- * Actions
    act,
  )
where

import AtCoder.LazySegTree (SegAct (..))
import Data.Bit (Bit (..))
import Data.Semigroup (stimes)
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM

-- | Monoid action \(f: x \rightarrow a\).
--
-- ==== __Example__
-- >>> import AtCoder.Extra.Monoid (SegAct(..), RangeWrite(..))
-- >>> import AtCoder.LazySegTree qualified as LSeg
-- >>> import Data.Bit (Bit (..))
-- >>> import Data.Semigroup (Product(..))
-- >>> seg <- LSeg.build @_ @(RangeWrite (Product Int)) @(Product Int) $ VU.generate 4 Product -- [0, 1, 2, 3]
-- >>> LSeg.applyIn seg 0 3 $ RangeWrite (Bit True, Product 5) -- [5, 5, 5, 3]
-- >>> getProduct <$> LSeg.prod seg 0 4
-- 375
--
-- @since 1.6.0.0
newtype RangeWrite a = RangeWrite (RangeWriteRepr a)
  deriving newtype
    ( -- | @since 1.6.0.0
      Eq,
      -- | @since 1.6.0.0
      Ord,
      -- | @since 1.6.0.0
      Show
    )

-- | `RangeWrite` internal representation. The first value represents if it is an identity action.
-- Tuples are not the fastest representation, but it's easier to implement
-- `Data.Vector.Unboxed.Unbox`.
--
-- @since 1.6.0.0
type RangeWriteRepr a = (Bit, a)

-- | \(O(1)\) Creates a new `RangeWrite` action.
--
-- @since 1.6.0.0
{-# INLINE new #-}
new :: a -> RangeWrite a
new = RangeWrite . (Bit True,)

-- | \(O(1)\) Retrieves the internal representation of `RangeWrite`.
--
-- @since 1.6.0.0
{-# INLINE unRangeWrite #-}
unRangeWrite :: RangeWrite a -> RangeWriteRepr a
unRangeWrite (RangeWrite a) = a

-- | \(O(1)\) Applies one-length range set: \(f: x \rightarrow y\).
--
-- @since 1.6.0.0
{-# INLINE act #-}
act :: RangeWrite a -> a -> a
act (RangeWrite (Bit True, !f)) _ = f
act (RangeWrite (Bit False, !_)) x = x

-- | \(O(1)\) Acts on @a@ with length in terms of `SegAct`. Be warned that it doesn't work well with
-- idempotent monoids such as `Max` or `Min`.
--
-- @since 1.6.0.0
{-# INLINE actWithLength #-}
actWithLength :: (Semigroup a) => Int -> RangeWrite a -> a -> a
actWithLength len (RangeWrite (Bit True, !f)) _ = stimes len f
actWithLength _ (RangeWrite (Bit False, !_)) x = x

-- | @since 1.6.0.0
instance Semigroup (RangeWrite a) where
  {-# INLINE (<>) #-}
  RangeWrite (Bit False, !_) <> old = old
  new_ <> _ = new_
  {-# INLINE stimes #-}
  stimes _ x = x

-- | @since 1.6.0.0
instance (Monoid a) => Monoid (RangeWrite a) where
  {-# INLINE mempty #-}
  mempty = RangeWrite (Bit False, mempty)
  {-# INLINE mconcat #-}
  -- find the first non-mempty
  mconcat [] = mempty
  mconcat (RangeWrite (Bit False, !_) : as) = mconcat as
  mconcat (a : _) = a

-- | @since 1.6.0.0
instance (Monoid a) => SegAct (RangeWrite a) a where
  {-# INLINE segActWithLength #-}
  segActWithLength = actWithLength

-- | @since 1.6.0.0
newtype instance VU.MVector s (RangeWrite a) = MV_RangeWrite (VU.MVector s (RangeWriteRepr a))

-- | @since 1.6.0.0
newtype instance VU.Vector (RangeWrite a) = V_RangeWrite (VU.Vector (RangeWriteRepr a))

-- | @since 1.6.0.0
deriving instance (VU.Unbox a) => VGM.MVector VUM.MVector (RangeWrite a)

-- | @since 1.6.0.0
deriving instance (VU.Unbox a) => VG.Vector VU.Vector (RangeWrite a)

-- | @since 1.6.0.0
instance (VU.Unbox a) => VU.Unbox (RangeWrite a)
