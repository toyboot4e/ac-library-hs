{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

-- | Three biggest values.
--
-- @since 1.6.0.0
module AtCoder.Extra.Monoid.Top3
  ( -- * Top3
    Top3 (..),
    Top3Repr,

    -- * Constructors
    new,
    unsafeNew,
    singleton,
    unTop3,

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

-- | Three biggest values.
--
-- @since 1.6.0.0
newtype Top3 a = Top3 (Top3Repr a)
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

-- | Internal representation of `Top3`.
--
-- @since 1.6.0.0
type Top3Repr a = (a, a, a)

-- | \(O(1)\) Creates a `Top3`.
--
-- @since 1.6.0.0
{-# INLINE new #-}
new :: (HasCallStack, Ord a) => a -> a -> a -> Top3 a
new a b c = case compare a b of
  -- a > b
  GT -> case compare a c of
    GT -> Top3 (a, max b c, min b c)
    EQ -> Top3 (a, c, b)
    LT -> Top3 (c, a, b)
  -- a = b
  EQ -> case compare a c of
    GT -> Top3 (a, b, c)
    EQ -> Top3 (a, b, c)
    LT -> Top3 (c, a, b)
  -- b > a
  LT -> case compare b c of
    GT -> Top3 (b, max a c, min a c)
    EQ -> Top3 (b, c, a)
    LT -> Top3 (c, b, a)

-- | \(O(1)\) Creates a `Top3` without sorting.
--
-- @since 1.6.0.0
{-# INLINE unsafeNew #-}
unsafeNew :: (HasCallStack) => a -> a -> a -> Top3 a
unsafeNew a b c = Top3 (a, b, c)

-- | \(O(1)\) Creates a `Top3` from a single value
--
-- @since 1.6.0.0
{-# INLINE singleton #-}
singleton :: (HasCallStack, Bounded a) => a -> Top3 a
singleton a = Top3 (a, minBound, minBound)

-- | \(O(1)\) Retrieves the internal representation.
--
-- @since 1.6.0.0
{-# INLINE unTop3 #-}
unTop3 :: Top3 a -> Top3Repr a
unTop3 (Top3 repr) = repr

-- | \(O(1)\) Inserts an item.
--
-- @since 1.6.0.0
{-# INLINE insert #-}
insert :: (Ord a) => a -> Top3 a -> Top3 a
insert x original@(Top3 (!a, !b, !c))
  | x > a = Top3 (x, a, b)
  | x > b = Top3 (a, x, b)
  | x > c = Top3 (a, b, x)
  | otherwise = original

-- | @since 1.6.0.0
instance (Ord a) => Semigroup (Top3 a) where
  {-# INLINE (<>) #-}
  Top3 (!a1, !a2, !a3) <> Top3 (!b1, !b2, !b3) = case compare a1 b1 of
    -- a1
    GT -> case compare a2 b1 of
      GT -> Top3 (a1, a2, max a3 b1)
      EQ -> Top3 (a1, a2, b1)
      LT -> Top3 (a1, b1, max a2 b2)
    -- a1, b1
    EQ -> Top3 (a1, b1, max a2 b2)
    -- b1
    LT -> case compare a1 b2 of
      GT -> Top3 (b1, a1, max a2 b2)
      EQ -> Top3 (b1, a1, b2)
      LT -> Top3 (b1, b2, max a1 b3)

-- | @since 1.6.0.0
instance (Ord a, Bounded a) => Monoid (Top3 a) where
  {-# INLINE mempty #-}
  mempty = Top3 (minBound, minBound, minBound)

-- | @since 1.6.0.0
newtype instance VU.MVector s (Top3 a) = MV_Top3 (VU.MVector s (Top3Repr a))

-- | @since 1.6.0.0
newtype instance VU.Vector (Top3 a) = V_Top3 (VU.Vector (Top3Repr a))

-- | @since 1.6.0.0
deriving instance (VU.Unbox a) => VGM.MVector VUM.MVector (Top3 a)

-- | @since 1.6.0.0
deriving instance (VU.Unbox a) => VG.Vector VU.Vector (Top3 a)

-- | @since 1.6.0.0
instance (VU.Unbox a) => VU.Unbox (Top3 a)
