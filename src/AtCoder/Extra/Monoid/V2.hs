{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

-- | A monoid acted on by `Mat2x2`, an affine transformation target.
--
-- @since 1.1.0.0
module AtCoder.Extra.Monoid.V2
  ( -- * V2
    V2 (..),
    V2Repr,

    -- * Constructor
    new,
    unV2,
  )
where

import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM

-- | A monoid acted on by `Mat2x2`, an affine transformation target.
--
-- @since 1.1.0.0
newtype V2 a = V2 (V2Repr a)
  deriving newtype
    ( -- | @since 1.1.0.0
      Eq,
      -- | @since 1.1.0.0
      Ord,
      -- | @since 1.1.0.0
      Show
    )

-- | `V2` internal representation. Tuples are not the fastest representation, but it's easier
-- to implement `Data.Vector.Unboxed.Unbox`.
--
-- @since 1.1.0.0
type V2Repr a = (a, a)

-- | \(O(1)\) Creates `V2` of length \(1\).
--
-- @since 1.1.0.0
{-# INLINE new #-}
new :: (Num a) => a -> V2 a
new !a = V2 (a, 1)

-- | \(O(1)\) Retrieves the value of `V2`, discarding the length information.
--
-- @since 1.1.0.0
{-# INLINE unV2 #-}
unV2 :: V2 a -> a
unV2 (V2 (!a, !_)) = a

-- | @since 1.1.0.0
instance (Num a) => Semigroup (V2 a) where
  {-# INLINE (<>) #-}
  (V2 (!a1, !a2)) <> (V2 (!b1, !b2)) = V2 (a', b')
    where
      !a' = a1 + b1
      !b' = a2 + b2

-- | @since 1.1.0.0
instance (Num a) => Monoid (V2 a) where
  {-# INLINE mempty #-}
  mempty = V2 (0, 0)

-- | @since 1.1.0.0
newtype instance VU.MVector s (V2 a) = MV_V2 (VU.MVector s (V2Repr a))

-- | @since 1.1.0.0
newtype instance VU.Vector (V2 a) = V_V2 (VU.Vector (V2Repr a))

-- | @since 1.1.0.0
deriving instance (VU.Unbox a) => VGM.MVector VUM.MVector (V2 a)

-- | @since 1.1.0.0
deriving instance (VU.Unbox a) => VG.Vector VU.Vector (V2 a)

-- | @since 1.1.0.0
instance (VU.Unbox a) => VU.Unbox (V2 a)
