{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

-- | Two biggest values.
--
-- @since 1.6.0.0
module AtCoder.Extra.Monoid.Top2
  ( -- * Top2
    Top2 (..),
    Top2Repr,

    -- * Constructors
    new,
    unsafeNew,
    singleton,
    unTop2,

    -- * Update
    insert,
  )
where

import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Stack (HasCallStack)
import Prelude hiding (length)

-- | Two biggest values.
--
-- @since 1.6.0.0
newtype Top2 a = Top2 (Top2Repr a)
  deriving newtype
    ( -- | @since 1.6.0.0
      Eq,
      -- | @since 1.6.0.0
      Ord,
      -- | @since 1.6.0.0
      Bounded,
      -- | @since 1.6.0.0
      Show
    )

-- | Internal representation of `Top2`.
--
-- @since 1.6.0.0
type Top2Repr a = (a, a)

-- | \(O(1)\) Creates a `Top2`, performing boundary check on input vector.
--
-- @since 1.6.0.0
{-# INLINE new #-}
new :: (HasCallStack, Ord a) => a -> a -> Top2 a
new a b = Top2 (max a b, min a b)

-- | \(O(1)\) Creates a `Top2`.
--
-- @since 1.6.0.0
{-# INLINE unsafeNew #-}
unsafeNew :: (HasCallStack) => a -> a -> Top2 a
unsafeNew a b = Top2 (a, b)

-- | \(O(1)\) Creates a `Top2` without sorting.
--
-- @since 1.6.0.0
{-# INLINE singleton #-}
singleton :: (HasCallStack, Bounded a) => a -> Top2 a
singleton a = Top2 (a, minBound)

-- | \(O(1)\) Retrieves the internal representation.
--
-- @since 1.6.0.0
{-# INLINE unTop2 #-}
unTop2 :: Top2 a -> Top2Repr a
unTop2 (Top2 repr) = repr

-- | \(O(1)\) Inserts an item.
--
-- @since 1.6.0.0
{-# INLINE insert #-}
insert :: (Ord a) => a -> Top2 a -> Top2 a
insert x original@(Top2 (!a, !b))
  | x > a = Top2 (x, a)
  | x > b = Top2 (a, x)
  | otherwise = original

-- | @since 1.6.0.0
instance (Ord a) => Semigroup (Top2 a) where
  {-# INLINE (<>) #-}
  Top2 (!a1, !a2) <> Top2 (!b1, !b2) = case compare a1 b1 of
    GT -> Top2 (a1, max a2 b1)
    EQ -> Top2 (a1, b1)
    LT -> Top2 (b1, max a1 b2)

-- | @since 1.6.0.0
instance (Ord a, Bounded a) => Monoid (Top2 a) where
  {-# INLINE mempty #-}
  mempty = Top2 (minBound, minBound)

-- | @since 1.6.0.0
newtype instance VU.MVector s (Top2 a) = MV_Top2 (VU.MVector s (Top2Repr a))

-- | @since 1.6.0.0
newtype instance VU.Vector (Top2 a) = V_Top2 (VU.Vector (Top2Repr a))

-- | @since 1.6.0.0
deriving instance (VU.Unbox a) => VGM.MVector VUM.MVector (Top2 a)

-- | @since 1.6.0.0
deriving instance (VU.Unbox a) => VG.Vector VU.Vector (Top2 a)

-- | @since 1.6.0.0
instance (VU.Unbox a) => VU.Unbox (Top2 a)
