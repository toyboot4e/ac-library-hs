-- | Extra math module.
--
-- ==== Examples
-- >>> import AtCoder.Extra.Math qualified as M
-- >>> import Data.Semigroup (Product(..), Sum(..))
-- >>> getProduct $ M.power (<>) 32 (Product 2)
-- 4294967296
--
-- >>> getProduct $ M.stimes' 32 (Product 2)
-- 4294967296
--
-- >>> getProduct $ M.mtimes' 32 (Product 2)
-- 4294967296
--
-- @since 1.0.0
module AtCoder.Extra.Math
  ( -- * Binary exponential
    power,
    stimes',
    mtimes',
  )
where

import Data.Bits ((.>>.))

-- TODO: add `HasCallStack` and provide with `unsafePower`.

-- | Calculates \(s^n\) with custom multiplication operator using the binary exponentiation
-- technique.
--
-- The internal implementation is taken from `Data.Semigroup.stimes`, but `power` uses strict
-- evaluation and is often much faster.
--
-- ==== Complexity
-- - \(O(\log n)\)
--
-- ==== Constraints
-- - \(n \gt 0\)
--
-- @since 1.0.0
{-# INLINE power #-}
power :: (a -> a -> a) -> Int -> a -> a
power op n0 x1
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

-- | Strict `Data.Semigroup.stimes`.
--
-- ==== Complexity
-- - \(O(\log n)\)
--
-- ==== Constraints
-- - \(n \gt 0\)
--
-- @since 1.0.0
{-# INLINE stimes' #-}
stimes' :: (Semigroup a) => Int -> a -> a
stimes' = power (<>)

-- | Strict `Data.Monoid.mtimes`.
--
-- ==== Complexity
-- - \(O(\log n)\)
--
-- ==== Constraints
-- - \(n \ge 0\)
--
-- @since 1.0.0
{-# INLINE mtimes' #-}
mtimes' :: (Monoid a) => Int -> a -> a
mtimes' n x = case compare n 0 of
  LT -> errorWithoutStackTrace "AtCoder.Extra.Math.mtimes': non-negative multiplier expected"
  EQ -> mempty
  GT -> power (<>) n x
