{-# LANGUAGE TypeFamilies #-}

-- | `AtCoder.LazySegTree.SegAct` instance of one-dimensional affine transformation
-- \(f: x \rightarrow a x + b\).
module AtCoder.Extra.Monoid.Affine1
  ( Affine1 (..),
    Affine1Repr,
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

-- | `AtCoder.LazySegTree.SegAct` instance of one-dimensional affine transformation
-- \(f: x \rightarrow a x + b\).
--
-- = Composition and dual
--
-- \((f_1 \diamond f_2) v := (f_1 . f_2) v\). If you need @foldr@ \([f_l, .., f_r]\) on a segment
-- tree, be sure to wrap `Affine1` with `Data.Monoid.Dual`.
newtype Affine1 a = Affine1 (Affine1Repr a)
  deriving newtype (Eq, Ord, Show)

-- | `Affine1` internal representation. Tuples are not the fastest representation, but it's easier
-- to implement `Data.Vector.Unboxed.Unbox`.
type Affine1Repr a = (a, a)

instance (Num a) => Semigroup (Affine1 a) where
  {-# INLINE (<>) #-}
  (Affine1 (!a1, !b1)) <> (Affine1 (!a2, !b2)) = Affine1 (a', b')
    where
      !a' = a1 * a2
      !b' = a1 * b2 + b1

instance (Num a) => Monoid (Affine1 a) where
  {-# INLINE mempty #-}
  mempty = Affine1 (1, 0)
  {-# INLINE mconcat #-}
  mconcat [] = mempty
  mconcat (x:xs) = foldl' (<>) x xs

instance (Num a) => SegAct (Affine1 a) a where
  {-# INLINE segActWithLength #-}
  segActWithLength !len (Affine1 (!a, !b)) !x = id $! a * x + b * fromIntegral len

instance (Integral a) => SegAct (Affine1 a) (Sum a) where
  {-# INLINE segActWithLength #-}
  segActWithLength !len (Affine1 (!a, !b)) (Sum !x) = Sum $! a * x + b * fromIntegral len

instance (Integral a) => SegAct (Affine1 a) (Product a) where
  {-# INLINE segActWithLength #-}
  segActWithLength !len (Affine1 (!a, !b)) (Product !x) = Product $! a * x + b * fromIntegral len

instance (Integral a) => SegAct (Affine1 a) (Min a) where
  {-# INLINE segActWithLength #-}
  segActWithLength !len (Affine1 (!a, !b)) (Min !x) = Min $! a * x + b * fromIntegral len

instance (Integral a) => SegAct (Affine1 a) (Max a) where
  {-# INLINE segActWithLength #-}
  segActWithLength !len (Affine1 (!a, !b)) (Max !x) = Max $! a * x + b * fromIntegral len

-- implementations for Duals
instance (Integral a) => SegAct (Dual (Affine1 a)) a where
  {-# INLINE segActWithLength #-}
  segActWithLength !len (Dual (Affine1 (!a, !b))) !x = id $! a * x + b * fromIntegral len

instance (Integral a) => SegAct (Dual (Affine1 a)) (Sum a) where
  {-# INLINE segActWithLength #-}
  segActWithLength !len (Dual (Affine1 (!a, !b))) (Sum !x) = Sum $! a * x + b * fromIntegral len

instance (Integral a) => SegAct (Dual (Affine1 a)) (Product a) where
  {-# INLINE segActWithLength #-}
  segActWithLength !len (Dual (Affine1 (!a, !b))) (Product !x) = Product $! a * x + b * fromIntegral len

instance (Integral a) => SegAct (Dual (Affine1 a)) (Min a) where
  {-# INLINE segActWithLength #-}
  segActWithLength !len (Dual (Affine1 (!a, !b))) (Min !x) = Min $! a * x + b * fromIntegral len

instance (Integral a) => SegAct (Dual (Affine1 a)) (Max a) where
  {-# INLINE segActWithLength #-}
  segActWithLength !len (Dual (Affine1 (!a, !b))) (Max !x) = Max $! a * x + b * fromIntegral len

newtype instance VU.MVector s (Affine1 a) = MV_Affine1 (VU.MVector s (Affine1Repr a))

newtype instance VU.Vector (Affine1 a) = V_Affine1 (VU.Vector (Affine1Repr a))

deriving instance (VU.Unbox a) => VGM.MVector VUM.MVector (Affine1 a)

deriving instance (VU.Unbox a) => VG.Vector VU.Vector (Affine1 a)

instance (VU.Unbox a) => VU.Unbox (Affine1 a)
