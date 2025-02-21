{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

-- | Monoid action \(f: x \rightarrow ax + b\).
--
-- - Use @Mat2x2@ if inverse operations are required, or if it's necessary to store the monoid
-- length in the acted monoid (@V2@).
--
-- @since 1.0.0.0
module AtCoder.Extra.Monoid.Affine1
  ( -- * Affine1
    Affine1 (..),
    Affine1Repr,

    -- * Constructors
    new,
    unAffine1,
    ident,
    zero,

    -- * Actions
    act,
  )
where

import AtCoder.Extra.Math qualified as ACEM
import AtCoder.LazySegTree (SegAct (..))
import Data.Coerce (coerce)
import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty (..))
import Data.Semigroup (Dual (..), Semigroup (..), Sum (..))
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM

-- | Monoid action \(f: x \rightarrow ax + b\).
--
-- - Use @Mat2x2@ if inverse operations are required, or if it's necessary to store the monoid
-- length in the acted monoid (@V2@).
--
-- ==== Composition and dual
-- The affine transformation acts as a left monoid action: \(f_2 (f_1 v) = (f_2 \circ f_1) v\). To
-- apply the leftmost transformation first in a segment tree, wrap `Affine1` in @Data.Monoid.Dual@.
--
-- ==== __Example__
-- >>> import AtCoder.Extra.Monoid (SegAct(..), Affine1(..))
-- >>> import AtCoder.LazySegTree qualified as LST
-- >>> seg <- LST.build @_ @(Affine1 Int) @(Sum Int) $ VU.generate 3 Sum -- [0, 1, 2]
-- >>> LST.applyIn seg 0 3 $ Affine1 (2, 1) -- [1, 3, 5]
-- >>> getSum <$> LST.allProd seg
-- 9
--
-- @since 1.0.0.0
newtype Affine1 a = Affine1 (Affine1Repr a)
  deriving newtype
    ( -- | @since 1.0.0.0
      Eq,
      -- | @since 1.0.0.0
      Ord,
      -- | @since 1.0.0.0
      Show
    )

-- | `Affine1` internal representation. Tuples are not the fastest representation, but it's easier
-- to implement `Data.Vector.Unboxed.Unbox`.
--
-- @since 1.0.0.0
type Affine1Repr a = (a, a)

-- | \(O(1)\) Creates a one-dimensional affine transformation: \(f: x \rightarrow a \times x + b\).
--
-- @since 1.0.0.0
{-# INLINE new #-}
new :: a -> a -> Affine1 a
new !a !b = Affine1 (a, b)

-- | \(O(1)\) Retrieves the two components of `Affine1`.
--
-- @since 1.1.0.0
{-# INLINE unAffine1 #-}
unAffine1 :: Affine1 a -> Affine1Repr a
unAffine1 (Affine1 a) = a

-- | \(O(1)\) Identity transformation.
--
-- @since 1.1.0.0
{-# INLINE ident #-}
ident :: (Num a) => Affine1 a
ident = Affine1 (1, 0)

-- | \(O(1)\) Transformation to zero.
--
-- @since 1.1.0.0
{-# INLINE zero #-}
zero :: (Num a) => Affine1 a
zero = Affine1 (0, 0)

-- | \(O(1)\) Applies the one-dimensional affine transformation \(f: x \rightarrow a \times x + b\).
--
-- @since 1.0.0.0
{-# INLINE act #-}
act :: (Num a) => Affine1 a -> a -> a
act (Affine1 (!a, !b)) x = a * x + b

-- | \(O(1)\) Acts on @a@ with length in terms of `SegAct`. Works for `Sum a` only.
--
-- @since 1.0.0.0
{-# INLINE actWithLength #-}
actWithLength :: (Num a) => Int -> Affine1 a -> a -> a
actWithLength len (Affine1 (!a, !b)) !x = a * x + b * fromIntegral len

-- | @since 1.0.0.0
instance (Num a) => Semigroup (Affine1 a) where
  {-# INLINE (<>) #-}
  (Affine1 (!a1, !b1)) <> (Affine1 (!a2, !b2)) = Affine1 (a', b')
    where
      !a' = a1 * a2
      !b' = a1 * b2 + b1
  {-# INLINE sconcat #-}
  sconcat (x :| xs) = foldl' (<>) x xs
  {-# INLINE stimes #-}
  stimes b = ACEM.power (<>) (fromIntegral b)

-- | @since 1.0.0.0
instance (Num a) => Monoid (Affine1 a) where
  {-# INLINE mempty #-}
  mempty = Affine1 (1, 0)
  {-# INLINE mconcat #-}
  mconcat [] = mempty
  mconcat (x : xs) = foldl' (<>) x xs

-- | @since 1.0.0.0
instance (Num a) => SegAct (Affine1 a) (Sum a) where
  {-# INLINE segActWithLength #-}
  segActWithLength !len f (Sum !x) = Sum $! actWithLength len f x

-- | @since 1.0.0.0
instance (Num a) => SegAct (Affine1 (Sum a)) (Sum a) where
  {-# INLINE segActWithLength #-}
  segActWithLength = actWithLength

-- | @since 1.0.0.0
instance (Num a) => SegAct (Dual (Affine1 a)) (Sum a) where
  {-# INLINE segActWithLength #-}
  segActWithLength !len (Dual f) (Sum !x) = Sum $! actWithLength len f x

-- | @since 1.0.0.0
instance (Num a) => SegAct (Dual (Affine1 (Sum a))) (Sum a) where
  {-# INLINE segActWithLength #-}
  segActWithLength !len (Dual f) (Sum !x) = Sum $! actWithLength len (coerce f) x

-- not works as SegAct for Product, Min, and Max.

-- | @since 1.0.0.0
newtype instance VU.MVector s (Affine1 a) = MV_Affine1 (VU.MVector s (Affine1Repr a))

-- | @since 1.0.0.0
newtype instance VU.Vector (Affine1 a) = V_Affine1 (VU.Vector (Affine1Repr a))

-- | @since 1.0.0.0
deriving instance (VU.Unbox a) => VGM.MVector VUM.MVector (Affine1 a)

-- | @since 1.0.0.0
deriving instance (VU.Unbox a) => VG.Vector VU.Vector (Affine1 a)

-- | @since 1.0.0.0
instance (VU.Unbox a) => VU.Unbox (Affine1 a)
