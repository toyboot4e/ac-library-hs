{-# LANGUAGE TypeFamilies #-}

-- | Opinionated zero-based \(n\)-dimensional index and their bounds.
module AtCoder.Extra.Ix0 where

-- | Zero-based \(n\)-dimensional bounds: \([0, d_0) \times [0, d_1) \times \cdots \times [0, d_{n - 1})\).
type Bounds0 i = i

-- | Zero-based \(n\)-dimensional index.
class Ix0 i where
  -- | Returns the size of the bounds: \(\prod_i d_i\).
  rangeSize0 :: Bounds0 i -> Int

  -- | Returns zero-based one-dimensional index, __without__ running boundary check.
  index0 :: Bounds0 i -> i -> Int

  -- | Returns whether an index is contained in bounds.
  inRange0 :: Bounds0 i -> i -> Bool

instance Ix0 Int where
  {-# INLINE rangeSize0 #-}
  rangeSize0 d1 = d1
  {-# INLINE index0 #-}
  index0 _ x1 = x1
  {-# INLINE inRange0 #-}
  inRange0 d1 x1 = 0 <= x1 && x1 < d1

instance Ix0 (Int, Int) where
  {-# INLINE rangeSize0 #-}
  rangeSize0 (!d2, !d1) = d2 * d1
  {-# INLINE index0 #-}
  index0 (!_, !d1) (!x2, !x1) = x2 * d1 + x1
  {-# INLINE inRange0 #-}
  inRange0 (!d2, !d1) (!x2, !x1) = 0 <= x2 && x2 < d2 && 0 <= x1 && x1 < d1

instance Ix0 (Int, Int, Int) where
  {-# INLINE rangeSize0 #-}
  rangeSize0 (!d3, !d2, !d1) = d3 * d2 * d1
  {-# INLINE index0 #-}
  index0 (!_, !d2, !d1) (!x3, !x2, !x1) = (x3 * d2 + x2) * d1 + x1
  {-# INLINE inRange0 #-}
  inRange0 (!d3, !d2, !d1) (!x3, !x2, !x1) = 0 <= x3 && x3 < d3 && 0 <= x2 && x2 < d2 && 0 <= x1 && x1 < d1

instance Ix0 (Int, Int, Int, Int) where
  {-# INLINE rangeSize0 #-}
  rangeSize0 (!d4, !d3, !d2, !d1) = d4 * d3 * d2 * d1
  {-# INLINE index0 #-}
  index0 (!_, !d3, !d2, !d1) (!x4, !x3, !x2, !x1) = ((x4 * d3 + x3) * d2 + x2) * d1 + x1
  {-# INLINE inRange0 #-}
  inRange0 (!d4, !d3, !d2, !d1) (!x4, !x3, !x2, !x1) = 0 <= x4 && x4 < d4 && 0 <= x3 && x3 < d3 && 0 <= x2 && x2 < d2 && 0 <= x1 && x1 < d1

instance Ix0 (Int, Int, Int, Int, Int) where
  {-# INLINE rangeSize0 #-}
  rangeSize0 (!d5, !d4, !d3, !d2, !d1) = d5 * d4 * d3 * d2 * d1
  {-# INLINE index0 #-}
  index0 (!_, !d4, !d3, !d2, !d1) (!x5, !x4, !x3, !x2, !x1) = (((x5 * d4 + x4) * d3 + x3) * d2 + x2) * d1 + x1
  {-# INLINE inRange0 #-}
  inRange0 (!d5, !d4, !d3, !d2, !d1) (!x5, !x4, !x3, !x2, !x1) = 0 <= x5 && x5 < d5 && 0 <= x4 && x4 < d4 && 0 <= x3 && x3 < d3 && 0 <= x2 && x2 < d2 && 0 <= x1 && x1 < d1

instance Ix0 (Int, Int, Int, Int, Int, Int) where
  {-# INLINE rangeSize0 #-}
  rangeSize0 (!d6, !d5, !d4, !d3, !d2, !d1) = d6 * d5 * d4 * d3 * d2 * d1
  {-# INLINE index0 #-}
  index0 (!_, !d5, !d4, !d3, !d2, !d1) (!x6, !x5, !x4, !x3, !x2, !x1) = ((((x6 * d5 + x5) * d4 + x4) * d3 + x3) * d2 + x2) * d1 + x1
  {-# INLINE inRange0 #-}
  inRange0 (!d6, !d5, !d4, !d3, !d2, !d1) (!x6, !x5, !x4, !x3, !x2, !x1) = 0 <= x6 && x6 < d6 && 0 <= x5 && x5 < d5 && 0 <= x4 && x4 < d4 && 0 <= x3 && x3 < d3 && 0 <= x2 && x2 < d2 && 0 <= x1 && x1 < d1
