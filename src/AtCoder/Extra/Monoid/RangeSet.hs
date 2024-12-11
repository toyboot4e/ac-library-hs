{-# LANGUAGE TypeFamilies #-}

-- | `AtCoder.LazySegTree.SegAct` instance of range set action. It can set an interval \([l, r)\) to
-- the same monoid \(x\) such as @Sum Int@.
--
-- While this monoid is a `SegAct` sample, be warned that it's not guaanteed to be correct.
module AtCoder.Extra.Monoid.RangeSet
  ( RangeSet (..),
    new,
    act,
  )
where

import AtCoder.Extra.Math qualified as ACEM
import AtCoder.LazySegTree (SegAct (..))
import Data.Semigroup (stimes)
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM

-- | `AtCoder.LazySegTree.SegAct` instance of range set action.
newtype RangeSet a = RangeSet (RangeSetRepr a)
  deriving newtype (Eq, Ord, Show)

-- | `RangeSet` internal representation. The first value represents if it is an identity action.
-- Tuples are not the fastest representation, but it's easier to implement
-- `Data.Vector.Unboxed.Unbox`.
type RangeSetRepr a = (Bool, a)

-- | Creates a new `RangeSet` action.
{-# INLINE new #-}
new :: a -> RangeSet a
new = RangeSet . (True,)

-- | Acts on @a@.
{-# INLINE act #-}
act :: RangeSet a -> a -> a
act (RangeSet (True, !f)) _ = f
act (RangeSet (False, !_)) x = x

-- | Acts on @a@ with length in terms of `SegAct`.
{-# INLINE actWithLength #-}
actWithLength :: (Semigroup a) => Int -> RangeSet a -> a -> a
actWithLength len (RangeSet (True, !f)) _ = ACEM.power len (<>) f
actWithLength _ (RangeSet (False, !_)) x = x

instance Semigroup (RangeSet a) where
  {-# INLINE (<>) #-}
  RangeSet (False, !_) <> old = old
  new_ <> _ = new_
  {-# INLINE stimes #-}
  stimes _ x = x

instance (Monoid a) => Monoid (RangeSet a) where
  {-# INLINE mempty #-}
  mempty = RangeSet (False, mempty)
  {-# INLINE mconcat #-}
  -- find the first non-mempty
  mconcat [] = mempty
  mconcat (RangeSet (False, !_) : as) = mconcat as
  mconcat (a : _) = a

instance (Monoid a) => SegAct (RangeSet a) a where
  {-# INLINE segActWithLength #-}
  segActWithLength = actWithLength

newtype instance VU.MVector s (RangeSet a) = MV_RangeSet (VU.MVector s (RangeSetRepr a))

newtype instance VU.Vector (RangeSet a) = V_RangeSet (VU.Vector (RangeSetRepr a))

deriving instance (VU.Unbox a) => VGM.MVector VUM.MVector (RangeSet a)

deriving instance (VU.Unbox a) => VG.Vector VU.Vector (RangeSet a)

instance (VU.Unbox a) => VU.Unbox (RangeSet a)
