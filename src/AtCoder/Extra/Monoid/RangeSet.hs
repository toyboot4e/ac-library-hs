{-# LANGUAGE TypeFamilies #-}

-- | Range set monoid action for \([l, r)\) intervals.
--
-- @since 1.0.0.0
module AtCoder.Extra.Monoid.RangeSet
  ( -- * RangeSet
    RangeSet (..),

    -- * Constructor
    new,

    -- * Action
    act,
  )
where

import AtCoder.Extra.Math qualified as ACEM
import AtCoder.LazySegTree (SegAct (..))
import Data.Bit (Bit (..))
import Data.Semigroup (stimes)
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM

-- | Range set monoid action.
--
-- ==== Example
-- >>> import AtCoder.Extra.Monoid (SegAct(..), RangeSet(..))
-- >>> import AtCoder.LazySegTree qualified as LST
-- >>> import Data.Bit (Bit (..))
-- >>> import Data.Semigroup (Product(..))
-- >>> seg <- LST.build @_ @(RangeSet (Product Int)) @(Product Int) $ VU.generate 4 Product -- [0, 1, 2, 3]
-- >>> LST.applyIn seg 0 3 $ RangeSet (Bit True, Product 5) -- [5, 5, 5, 3]
-- >>> getProduct <$> LST.prod seg 0 4
-- 375
--
-- @since 1.0.0.0
newtype RangeSet a = RangeSet (RangeSetRepr a)
  deriving newtype
    ( -- | @since 1.0.0.0
      Eq,
      -- | @since 1.0.0.0
      Ord,
      -- | @since 1.0.0.0
      Show
    )

-- | `RangeSet` internal representation. The first value represents if it is an identity action.
-- Tuples are not the fastest representation, but it's easier to implement
-- `Data.Vector.Unboxed.Unbox`.
--
-- @since 1.0.0.0
type RangeSetRepr a = (Bit, a)

-- | Creates a new `RangeSet` action.
--
-- @since 1.0.0.0
{-# INLINE new #-}
new :: a -> RangeSet a
new = RangeSet . (Bit True,)

-- | Applies one-length range set: \(f: x \rightarrow y\).
--
-- @since 1.0.0.0
{-# INLINE act #-}
act :: RangeSet a -> a -> a
act (RangeSet (Bit True, !f)) _ = f
act (RangeSet (Bit False, !_)) x = x

-- | Acts on @a@ with length in terms of `SegAct`.
--
-- @since 1.0.0.0
{-# INLINE actWithLength #-}
actWithLength :: (Semigroup a) => Int -> RangeSet a -> a -> a
actWithLength len (RangeSet (Bit True, !f)) _ = ACEM.power (<>) len f
actWithLength _ (RangeSet (Bit False, !_)) x = x

-- | @since 1.0.0.0
instance Semigroup (RangeSet a) where
  {-# INLINE (<>) #-}
  RangeSet (Bit False, !_) <> old = old
  new_ <> _ = new_
  {-# INLINE stimes #-}
  stimes _ x = x

-- | @since 1.0.0.0
instance (Monoid a) => Monoid (RangeSet a) where
  {-# INLINE mempty #-}
  mempty = RangeSet (Bit False, mempty)
  {-# INLINE mconcat #-}
  -- find the first non-mempty
  mconcat [] = mempty
  mconcat (RangeSet (Bit False, !_) : as) = mconcat as
  mconcat (a : _) = a

-- | @since 1.0.0.0
instance (Monoid a) => SegAct (RangeSet a) a where
  {-# INLINE segActWithLength #-}
  segActWithLength = actWithLength

-- | @since 1.0.0.0
newtype instance VU.MVector s (RangeSet a) = MV_RangeSet (VU.MVector s (RangeSetRepr a))

-- | @since 1.0.0.0
newtype instance VU.Vector (RangeSet a) = V_RangeSet (VU.Vector (RangeSetRepr a))

-- | @since 1.0.0.0
deriving instance (VU.Unbox a) => VGM.MVector VUM.MVector (RangeSet a)

-- | @since 1.0.0.0
deriving instance (VU.Unbox a) => VG.Vector VU.Vector (RangeSet a)

-- | @since 1.0.0.0
instance (VU.Unbox a) => VU.Unbox (RangeSet a)
