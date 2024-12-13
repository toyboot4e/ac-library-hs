{-# LANGUAGE TypeFamilies #-}

-- | `AtCoder.LazySegTree.SegAct` instance of one-dimensional affine transformation
-- \(f: x \rightarrow a x + b\).
module AtCoder.Extra.Monoid.Affine1
  ( Affine1 (..),
    Affine1Repr,
    new,
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

-- Tuple is not the fastest representation, but it's easier to implement `Unbox`.

-- | `AtCoder.LazySegTree.SegAct` instance of one-dimensional affine transformation
-- \(f: x \rightarrow a x + b\).
--
-- = Composition and dual
--
-- \((f_1 \cdot f_2) v := (f_1 . f_2) v\). If you need @foldr@ \([f_l, .., f_r]\) on a segment
-- tree, be sure to wrap `Affine1` with `Data.Monoid.Dual`.
--
-- = Example
-- >>> import AtCoder.Extra.Monoid (SegAct(..), Affine1(..))
-- >>> import AtCoder.LazySegTree qualified as LST
-- >>> seg <- LST.build @_ @(Affine1 Int) @(Sum Int) $ VU.generate 3 Sum -- [0, 1, 2]
-- >>> LST.applyIn seg 0 3 $ Affine1 (2, 1) -- [1, 3, 5]
-- >>> getSum <$> LST.allProd seg
-- 9
newtype Affine1 a = Affine1 (Affine1Repr a)
  deriving newtype (Eq, Ord, Show)

-- | `Affine1` internal representation. Tuples are not the fastest representation, but it's easier
-- to implement `Data.Vector.Unboxed.Unbox`.
type Affine1Repr a = (a, a)

-- | Creates `Affine1`.
{-# INLINE new #-}
new :: a -> a -> Affine1 a
new !a !b = Affine1 (a, b)

-- | Acts on @a@.
{-# INLINE act #-}
act :: (Num a) => Affine1 a -> a -> a
act (Affine1 (!a, !b)) x = a * x + b

-- | Acts on @a@ with length in terms of `SegAct`. Works for `Sum a` only.
{-# INLINE actWithLength #-}
actWithLength :: (Num a) => Int -> Affine1 a -> a -> a
actWithLength len (Affine1 (!a, !b)) !x = a * x + b * fromIntegral len

instance (Num a) => Semigroup (Affine1 a) where
  {-# INLINE (<>) #-}
  (Affine1 (!a1, !b1)) <> (Affine1 (!a2, !b2)) = Affine1 (a', b')
    where
      !a' = a1 * a2
      !b' = a1 * b2 + b1
  {-# INLINE sconcat #-}
  sconcat (x :| xs) = foldl' (<>) x xs
  {-# INLINE stimes #-}
  stimes b = ACEM.power (fromIntegral b) (<>)

instance (Num a) => Monoid (Affine1 a) where
  {-# INLINE mempty #-}
  mempty = Affine1 (1, 0)
  {-# INLINE mconcat #-}
  mconcat [] = mempty
  mconcat (x : xs) = foldl' (<>) x xs

instance (Num a) => SegAct (Affine1 a) (Sum a) where
  {-# INLINE segActWithLength #-}
  segActWithLength !len f (Sum !x) = Sum $! actWithLength len f x

instance (Num a) => SegAct (Affine1 (Sum a)) (Sum a) where
  {-# INLINE segActWithLength #-}
  segActWithLength = actWithLength

instance (Num a) => SegAct (Dual (Affine1 a)) (Sum a) where
  {-# INLINE segActWithLength #-}
  segActWithLength !len (Dual f) (Sum !x) = Sum $! actWithLength len f x

instance (Num a) => SegAct (Dual (Affine1 (Sum a))) (Sum a) where
  {-# INLINE segActWithLength #-}
  segActWithLength !len (Dual f) (Sum !x) = Sum $! actWithLength len (coerce f) x

-- not works as SegAct for Product, Min, and Max.

newtype instance VU.MVector s (Affine1 a) = MV_Affine1 (VU.MVector s (Affine1Repr a))

newtype instance VU.Vector (Affine1 a) = V_Affine1 (VU.Vector (Affine1Repr a))

deriving instance (VU.Unbox a) => VGM.MVector VUM.MVector (Affine1 a)

deriving instance (VU.Unbox a) => VG.Vector VU.Vector (Affine1 a)

instance (VU.Unbox a) => VU.Unbox (Affine1 a)
