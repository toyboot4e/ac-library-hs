{-# LANGUAGE TypeFamilies #-}

-- | `AtCoder.LazySegTree.SegAct` instance of 2D affine transformation \(f: x \rightarrow a x + b\).
module AtCoder.Extra.Monoid.Affine2d
  ( Affine2d (..),
    Affine2dRepr,
  )
where

import AtCoder.LazySegTree (SegAct (..))
import Data.Foldable (foldl')
import Data.Semigroup (Max (..), Min (..), Product (..), Sum (..), Dual(..))
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM

-- Tuple is not the fastest representation, but it's easier to implement `Unbox`.

-- | `AtCoder.LazySegTree.SegAct` instance of 2D affine transformation \(f: x \rightarrow a x + b\).
--
-- = Composition and dual
--
-- \((f_1 \diamond f_2) v := (f_1 . f_2) v\). If you need @foldr@ \([f_l, .., f_r]\) on a segment
-- tree, be sure to wrap `Affine2d` with `Data.Monoid.Dual`.
newtype Affine2d a = Affine2d (Affine2dRepr a)
  deriving newtype (Eq, Ord, Show)

-- | `Affine2d` internal representation. Tuples are not the fastest representation, but it's easier
-- to implement `Data.Vector.Unboxed.Unbox`.
type Affine2dRepr a = (a, a)

instance (Num a) => Semigroup (Affine2d a) where
  {-# INLINE (<>) #-}
  (Affine2d (!a1, !b1)) <> (Affine2d (!a2, !b2)) = Affine2d (a', b')
    where
      !a' = a1 * a2
      !b' = a1 * b2 + b1

instance (Num a) => Monoid (Affine2d a) where
  {-# INLINE mempty #-}
  mempty = Affine2d (1, 0)
  {-# INLINE mconcat #-}
  mconcat [] = mempty
  mconcat (x:xs) = foldl' (<>) x xs

instance (Num a) => SegAct (Affine2d a) a where
  {-# INLINE segActWithLength #-}
  segActWithLength !len (Affine2d (!a, !b)) !x = id $! a * x + b * fromIntegral len

instance (Integral a) => SegAct (Affine2d a) (Sum a) where
  {-# INLINE segActWithLength #-}
  segActWithLength !len (Affine2d (!a, !b)) (Sum !x) = Sum $! a * x + b * fromIntegral len

instance (Integral a) => SegAct (Affine2d a) (Product a) where
  {-# INLINE segActWithLength #-}
  segActWithLength !len (Affine2d (!a, !b)) (Product !x) = Product $! a * x + b * fromIntegral len

instance (Integral a) => SegAct (Affine2d a) (Min a) where
  {-# INLINE segActWithLength #-}
  segActWithLength !len (Affine2d (!a, !b)) (Min !x) = Min $! a * x + b * fromIntegral len

instance (Integral a) => SegAct (Affine2d a) (Max a) where
  {-# INLINE segActWithLength #-}
  segActWithLength !len (Affine2d (!a, !b)) (Max !x) = Max $! a * x + b * fromIntegral len

-- implementations for Duals
instance (Integral a) => SegAct (Dual (Affine2d a)) a where
  {-# INLINE segActWithLength #-}
  segActWithLength !len (Dual (Affine2d (!a, !b))) !x = id $! a * x + b * fromIntegral len

instance (Integral a) => SegAct (Dual (Affine2d a)) (Sum a) where
  {-# INLINE segActWithLength #-}
  segActWithLength !len (Dual (Affine2d (!a, !b))) (Sum !x) = Sum $! a * x + b * fromIntegral len

instance (Integral a) => SegAct (Dual (Affine2d a)) (Product a) where
  {-# INLINE segActWithLength #-}
  segActWithLength !len (Dual (Affine2d (!a, !b))) (Product !x) = Product $! a * x + b * fromIntegral len

instance (Integral a) => SegAct (Dual (Affine2d a)) (Min a) where
  {-# INLINE segActWithLength #-}
  segActWithLength !len (Dual (Affine2d (!a, !b))) (Min !x) = Min $! a * x + b * fromIntegral len

instance (Integral a) => SegAct (Dual (Affine2d a)) (Max a) where
  {-# INLINE segActWithLength #-}
  segActWithLength !len (Dual (Affine2d (!a, !b))) (Max !x) = Max $! a * x + b * fromIntegral len

newtype instance VU.MVector s (Affine2d a) = MV_Affine2d (VU.MVector s (Affine2dRepr a))

newtype instance VU.Vector (Affine2d a) = V_Affine2d (VU.Vector (Affine2dRepr a))

deriving instance (VU.Unbox a) => VGM.MVector VUM.MVector (Affine2d a)

deriving instance (VU.Unbox a) => VG.Vector VU.Vector (Affine2d a)

instance (VU.Unbox a) => VU.Unbox (Affine2d a)
