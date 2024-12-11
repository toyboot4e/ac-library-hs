-- | Extra math module.
module AtCoder.Extra.Math
  ( power,
    stimes',
    mtimes',
  )
where

import Data.Bits ((.>>.))

-- | \(O(\log n)\) Calculates \(s^n\) with custom multiplication operator using the binary lifting
-- technique.
--
-- The internal implementation is taken from `Data.Semigroup.stimes`, but `powers` uses strict
-- evaluation and is often much faster.
--
-- = Constraints
-- - \(n \gt 0\)
{-# INLINE power #-}
power :: Int -> (a -> a -> a) -> a -> a
power n0 op x1
  | n0 <= 0 = errorWithoutStackTrace "AtCoder.Extra.Math.power: positive multiplier expected"
  | otherwise = f x1 n0
  where
    f !x !n
      | even n = f (x `op` x) (n .>>. 1)
      | n == 1 = x
      | otherwise = g (x `op` x) (n .>>. 1) x
    g !x !n !z
      | even n = g (x `op` x) (n .>>. 1) z
      | n == 1 = x `op` z
      | otherwise = g (x `op` x) (n .>>. 1) (x `op` z)

-- | \(O(\log n)\) Strict `Data.Semigroup.stimes`.
--
-- = Constraints
-- - \(n \gt 0\)
{-# INLINE stimes' #-}
stimes' :: (Semigroup a) => Int -> a -> a
stimes' n = power n (<>)

-- | \(O(\log n)\) Strict `Data.Monoid.mtimes`.
--
-- = Constraints
-- - \(n \ge 0\)
{-# INLINE mtimes' #-}
mtimes' :: (Monoid a) => Int -> a -> a
mtimes' n0 x1 = case compare n0 0 of
  LT -> errorWithoutStackTrace "AtCoder.Extra.Math.mtimes': zero or positive multiplier expected"
  EQ -> mempty
  GT -> power n0 (<>) x1

