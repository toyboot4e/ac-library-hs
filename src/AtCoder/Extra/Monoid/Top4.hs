{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

-- | Four biggest values.
--
-- @since 1.6.0.0
module AtCoder.Extra.Monoid.Top4
  ( -- * Top4
    Top4 (..),
    Top4Repr,

    -- * Constructors
    new,
    unsafeNew,
    singleton,
    unTop4,

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

-- | Four biggest values.
--
-- @since 1.6.0.0
newtype Top4 a = Top4 (Top4Repr a)
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

-- | Internal representation of `Top4`.
--
-- @since 1.6.0.0
type Top4Repr a = (a, a, a, a)

-- | \(O(1)\) Creates a `Top4`.
--
-- @since 1.6.0.0
{-# INLINE new #-}
new :: (HasCallStack, Ord a) => a -> a -> a -> a -> Top4 a
new a b c d =
  let (!a1, !b1) = if a >= b then (a, b) else (b, a)
      (!c1, !d1) = if c >= d then (c, d) else (d, c)
      (!a2, !c2) = if a1 >= c1 then (a1, c1) else (c1, a1)
      (!b2, !d2) = if b1 >= d1 then (b1, d1) else (d1, b1)
      (!b3, !c3) = if b2 >= c2 then (b2, c2) else (c2, b2)
   in Top4 (a2, b3, c3, d2)

-- | \(O(1)\) Creates a `Top4` without sorting.
--
-- @since 1.6.0.0
{-# INLINE unsafeNew #-}
unsafeNew :: (HasCallStack) => a -> a -> a -> a -> Top4 a
unsafeNew a b c d = Top4 (a, b, c, d)

-- | \(O(1)\) Creates a `Top4` from a single value
--
-- @since 1.6.0.0
{-# INLINE singleton #-}
singleton :: (HasCallStack, Bounded a) => a -> Top4 a
singleton a = Top4 (a, minBound, minBound, minBound)

-- | \(O(1)\) Retrieves the internal representation.
--
-- @since 1.6.0.0
{-# INLINE unTop4 #-}
unTop4 :: Top4 a -> Top4Repr a
unTop4 (Top4 repr) = repr

-- | \(O(1)\) Inserts an item.
--
-- @since 1.6.0.0
{-# INLINE insert #-}
insert :: (Ord a) => a -> Top4 a -> Top4 a
insert x original@(Top4 (!a, !b, !c, !d))
  | x > a = Top4 (x, a, b, c)
  | x > b = Top4 (a, x, b, c)
  | x > c = Top4 (a, b, x, c)
  | x > d = Top4 (a, b, c, x)
  | otherwise = original

-- | @since 1.6.0.0
instance (Ord a) => Semigroup (Top4 a) where
  {-# INLINE (<>) #-}
  Top4 (!a1, !a2, !a3, !a4) <> Top4 (!b1, !b2, !b3, !b4) = case compare a1 b1 of
    -- a1
    GT -> case compare a2 b1 of
      -- a1, a2
      GT -> case compare a3 b1 of
        GT -> Top4 (a1, a2, a3, max a4 b1)
        EQ -> Top4 (a1, a2, a3, b1)
        LT -> Top4 (a1, a2, b1, max a3 b2)
      -- a1, a2, b1
      EQ -> Top4 (a1, a2, b1, max a3 b2)
      -- a1, b1
      LT -> case compare a2 b2 of
        GT -> Top4 (a1, b1, a2, max a3 b2)
        EQ -> Top4 (a1, b1, a2, b2)
        LT -> Top4 (a1, b1, b2, max a2 b3)
    -- a1, b1
    EQ -> case compare a2 b2 of
      GT -> Top4 (a1, b1, a2, max a3 b2)
      EQ -> Top4 (a1, b1, a2, b2)
      LT -> Top4 (a1, b1, b2, max a2 b3)
    -- b1
    LT -> case compare a1 b2 of
      -- b1, a1
      GT -> case compare a2 b2 of
        GT -> Top4 (b1, a1, a2, max a3 b2)
        EQ -> Top4 (b1, a1, a2, b2)
        LT -> Top4 (b1, a1, b2, max a2 b3)
      -- b1, a1, b2
      EQ -> Top4 (b1, a1, a2, max a3 b2)
      -- b1, b2
      LT -> case compare a1 b3 of
        GT -> Top4 (b1, b2, a1, max a2 b3)
        EQ -> Top4 (b1, b2, a1, b3)
        LT -> Top4 (b1, b2, b3, max a1 b4)

-- | @since 1.6.0.0
instance (Ord a, Bounded a) => Monoid (Top4 a) where
  {-# INLINE mempty #-}
  mempty = Top4 (minBound, minBound, minBound, minBound)

-- | @since 1.6.0.0
newtype instance VU.MVector s (Top4 a) = MV_Top4 (VU.MVector s (Top4Repr a))

-- | @since 1.6.0.0
newtype instance VU.Vector (Top4 a) = V_Top4 (VU.Vector (Top4Repr a))

-- | @since 1.6.0.0
deriving instance (VU.Unbox a) => VGM.MVector VUM.MVector (Top4 a)

-- | @since 1.6.0.0
deriving instance (VU.Unbox a) => VG.Vector VU.Vector (Top4 a)

-- | @since 1.6.0.0
instance (VU.Unbox a) => VU.Unbox (Top4 a)
