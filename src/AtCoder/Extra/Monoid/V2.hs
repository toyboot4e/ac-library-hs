{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

-- | A monoid acted on by `Mat2x2` or `Affine1`, an affine transformation target.
--
-- ==== As an `Affine1` action target
-- Compared to `Sum`, `V2` hold the length in the second value.
--
-- @since 1.1.0.0
module AtCoder.Extra.Monoid.V2
  ( -- * V2
    V2 (..),
    V2Repr,

    -- * Constructors
    new,
    unV2,
    zero,
    isZero,
  )
where

import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM

-- | A monoid acted on by `Mat2x2`, an affine transformation target.
--
-- ==== __Example__
-- >>> import AtCoder.Extra.Monoid.Mat2x2 (Mat2x2(..))
-- >>> import AtCoder.Extra.Monoid.Mat2x2 qualified as Mat2x2
-- >>> import AtCoder.Extra.Monoid.V2 (V2(..))
-- >>> import AtCoder.Extra.Monoid.V2 qualified as V2
-- >>> import AtCoder.LazySegTree qualified as LST
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> seg <- LST.build @_ @(Mat2x2 Int) @(V2 Int) . VU.map V2.new $ VU.fromList [1, 2, 3, 4]
-- >>> LST.applyIn seg 1 3 $ Mat2x2.new 2 1 -- [1, 5, 7, 4]
-- >>> V2.unV2 <$> LST.prod seg 1 3
-- 12
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

-- | \(O(1)\) Creates a `V2` of length \(1\).
--
-- @since 1.1.0.0
{-# INLINE new #-}
new :: (Num a) => a -> V2 a
new !a = V2 (a, 1)

-- | \(O(1)\) Creates a `V2` of length \(0\), i.e., `mempty`. Note that this value does not change
-- on affine transformation.
--
-- @since 1.2.2.0
{-# INLINE zero #-}
zero :: (Num a) => V2 a
zero = V2 (0, 0)

-- | \(O(1)\) Retrieves the value of `V2`, discarding the length information.
--
-- @since 1.1.0.0
{-# INLINE unV2 #-}
unV2 :: V2 a -> a
unV2 (V2 (!a, !_)) = a

-- | \(O(1)\) Returns whether the `V2` is equal to `zero`
--
-- @since 1.2.2.0
{-# INLINE isZero #-}
isZero :: (Num a, Eq a) => V2 a -> Bool
isZero = (== zero)

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
