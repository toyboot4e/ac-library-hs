{-# LANGUAGE TypeFamilies #-}

-- | Range transformation monoid action for \([l, r)\) intervals: \(f: x \rightarrow ax + b\). It is
-- less efficient to @Affine1@ but works with inverse opereations.
--
-- @since 1.1.0.0
module AtCoder.Extra.Monoid.Mat2x2
  ( -- * Mat2x2
    Mat2x2 (..),
    Mat2x2Repr,

    -- * Constructor
    new,
    unMat2x2,

    -- * Action
    act,

    -- * Operators
    -- zero,
    ident,
    inv,
  )
where

import AtCoder.Extra.Math qualified as ACEM
import AtCoder.Extra.Monoid.V2 (V2(..))
import AtCoder.LazySegTree (SegAct (..))
import Data.Semigroup (Dual (..), Semigroup (..))
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM

-- | Range transformation monoid action for \([l, r)\) intervals: \(f: x \rightarrow ax + b\). It is
-- less efficient to @Affine1@ but works with inverse opereations.
--
-- The matrix is considered to be left semigroup action: \(m_2 (m_1 v) = (m_2 \circ m_1) v\).
--
-- ==== Composition and dual
-- `Semigroup` for `Mat2x2` is implemented like function composition, and rightmost affine
-- transformation is applied first: \((f_1 \circ f_2) v := f_1 (f_2(v))\). If you need 'foldr'
-- of \([f_l, f_{l+1}, .., f_r)\) on a segment tree, be sure to wrap `Mat2x2` in
-- @Data.Monoid.Dual@.
--
-- ==== __Example__
-- >>> import AtCoder.Extra.Monoid.Mat2x2 qualified as Mat2x2
-- >>> import AtCoder.Extra.Monoid.V2 qualified as V2
-- >>> import AtCoder.Extra.Monoid (SegAct(..), Mat2x2(..), V2(..))
-- >>> import AtCoder.LazySegTree qualified as LST
-- >>> seg <- LST.build @_ @(Mat2x2 Int) @(V2 Int) $ VU.generate 3 V2.new -- [0, 1, 2]
-- >>> LST.applyIn seg 0 3 $ Mat2x2.new 2 1 -- [1, 3, 5]
-- >>> V2.unV2 <$> LST.allProd seg
-- 9
--
-- @since 1.1.0.0
newtype Mat2x2 a = Mat2x2 (Mat2x2Repr a)
  deriving newtype
    ( -- | @since 1.1.0.0
      Eq,
      -- | @since 1.1.0.0
      Ord,
      -- | @since 1.1.0.0
      Show
    )

-- | `Mat2x2` internal representation. Tuples are not the fastest representation, but it's easier
-- to implement `Data.Vector.Unboxed.Unbox`.
--
-- @since 1.1.0.0
type Mat2x2Repr a = (a, a, a, a)

-- | \(O(1)\) Creates an affine transformation: \(f: x \rightarrow a \times x + b\).
--
-- @since 1.1.0.0
{-# INLINE new #-}
new :: (Num a) => a -> a -> Mat2x2 a
new !a !b = Mat2x2 (a, b, 0, 1)

-- | \(O(1)\) Retrieves the four components of `Mat2x2`.
--
-- @since 1.1.0.0
{-# INLINE unMat2x2 #-}
unMat2x2 :: Mat2x2 a -> Mat2x2Repr a
unMat2x2 (Mat2x2 a) = a

-- | \(O(1)\) Multiplies `Mat2x2` to `V2`.
{-# INLINE mulMV #-}
mulMV :: (Num a) => Mat2x2 a -> V2 a -> V2 a
mulMV (Mat2x2 (!a11, !a12, !a21, !a22)) (V2 (!x1, !x2)) = V2 (a', b')
  where
    !a' = a11 * x1 + a12 * x2
    !b' = a21 * x1 + a22 * x2

-- | \(O(1)\) Multiplies `Mat2x2` to `Mat2x2`.
{-# INLINE mulMM #-}
mulMM :: (Num a) => Mat2x2 a -> Mat2x2 a -> Mat2x2 a
mulMM (Mat2x2 (!a11, !a12, !a21, !a22)) (Mat2x2 (!b11, !b12, !b21, !b22)) = Mat2x2 (c11, c12, c21, c22)
  where
    !c11 = a11 * b11 + a12 * b21
    !c12 = a11 * b12 + a12 * b22
    !c21 = a21 * b11 + a22 * b21
    !c22 = a21 * b12 + a22 * b22

-- | \(O(1)\) Multiplies `Mat2x2` to `V2`.
--
-- @since 1.1.0.0
{-# INLINE act #-}
act :: (Num a) => Mat2x2 a -> V2 a -> V2 a
act = mulMV

-- | \(O(1)\) Identity transformation.
--
-- @since 1.1.0.0
{-# INLINE ident #-}
ident :: (Num a) => Mat2x2 a
ident = Mat2x2 (1, 0, 0, 1)

-- | \(O(1)\) Returns 2x2 unit matrix, based on `Fractional` instance.
{-# INLINE inv #-}
inv :: (Fractional e) => Mat2x2 e -> Mat2x2 e
inv (Mat2x2 (!a, !b, !c, !d)) = Mat2x2 (a', b', c', d')
  where
    !r = recip $ a * d - b * c
    !a' = r * d
    !b' = r * (-b)
    !c' = r * (-c)
    !d' = r * a

-- | @since 1.1.0.0
instance (Num a) => Semigroup (Mat2x2 a) where
  {-# INLINE (<>) #-}
  (<>) = mulMM
  {-# INLINE stimes #-}
  stimes = ACEM.stimes' . fromIntegral

-- | @since 1.1.0.0
instance (Num a) => Monoid (Mat2x2 a) where
  {-# INLINE mempty #-}
  mempty = ident

-- | @since 1.1.0.0
instance (Num a) => SegAct (Mat2x2 a) (V2 a) where
  {-# INLINE segAct #-}
  segAct = mulMV

-- | @since 1.1.0.0
instance (Num a) => SegAct (Dual (Mat2x2 a)) (V2 a) where
  {-# INLINE segAct #-}
  segAct (Dual f) = mulMV f

-- | @since 1.1.0.0
newtype instance VU.MVector s (Mat2x2 a) = MV_Mat2x2 (VU.MVector s (Mat2x2Repr a))

-- | @since 1.1.0.0
newtype instance VU.Vector (Mat2x2 a) = V_Mat2x2 (VU.Vector (Mat2x2Repr a))

-- | @since 1.1.0.0
deriving instance (VU.Unbox a) => VGM.MVector VUM.MVector (Mat2x2 a)

-- | @since 1.1.0.0
deriving instance (VU.Unbox a) => VG.Vector VU.Vector (Mat2x2 a)

-- | @since 1.1.0.0
instance (VU.Unbox a) => VU.Unbox (Mat2x2 a)
