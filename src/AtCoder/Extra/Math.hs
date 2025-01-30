-- | Extra math module.
--
-- @since 1.0.0.0
module AtCoder.Extra.Math
  ( -- * Re-exports from the internal math module
    isPrime32,
    ACIM.invGcd,
    primitiveRoot32,

    -- * Binary exponentiation

    -- | ==== __Examples__
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
    power,
    stimes',
    mtimes',
  )
where

import AtCoder.Internal.Assert qualified as ACIA
import AtCoder.Internal.Math qualified as ACIM
import Data.Bits ((.>>.))
import GHC.Stack (HasCallStack)

-- | \(O(k \log^3 n) (k = 3)\). Returns whether the given `Int` value is a prime number.
--
-- ==== Constraints
-- - \(n < 4759123141 (2^{32} < 4759123141)\), otherwise the return value can lie
--   (see [Wikipedia](https://en.wikipedia.org/wiki/Miller%E2%80%93Rabin_primality_test#Testing_against_small_sets_of_bases)).
--
--
-- @since 1.1.0.0
{-# INLINE isPrime32 #-}
isPrime32 :: (HasCallStack) => Int -> Bool
isPrime32 x = ACIM.isPrime x
  where
    !_ = ACIA.runtimeAssert (x < 4759123141) $ "AtCoder.Extra.Math.isPrime32: given too large number `" ++ show x ++ "`"

-- | Returns the primitive root of the given `Int`.
--
-- ==== Constraints
-- - The input must be a prime number.
-- - The input must be less than \(2^32\).
--
-- @since 1.2.0.0
{-# INLINE primitiveRoot32 #-}
primitiveRoot32 :: (HasCallStack) => Int -> Int
primitiveRoot32 x = ACIM.primitiveRoot x
  where
    !_ = ACIA.runtimeAssert (x < (1 .>>. 32)) $ "AtCoder.Extra.Math.primitiveRoot32: given too large number `" ++ show x ++ "`"

-- | Calculates \(x^n\) with custom multiplication operator using the binary exponentiation
-- technique.
--
-- The internal implementation is taken from @Data.Semigroup.stimes@, but `power` uses strict
-- evaluation and is often much faster.
--
-- ==== Complexity
-- - \(O(\log n)\)
--
-- ==== Constraints
-- - \(n \gt 0\)
--
-- @since 1.0.0.0
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

-- | Strict variant of @Data.Semigroup.stimes@.
--
-- ==== Complexity
-- - \(O(\log n)\)
--
-- ==== Constraints
-- - \(n \gt 0\)
--
-- @since 1.0.0.0
{-# INLINE stimes' #-}
stimes' :: (Semigroup a) => Int -> a -> a
stimes' = power (<>)

-- | Strict variant of @Data.Monoid.mtimes@.
--
-- ==== Complexity
-- - \(O(\log n)\)
--
-- ==== Constraints
-- - \(n \ge 0\)
--
-- @since 1.0.0.0
{-# INLINE mtimes' #-}
mtimes' :: (Monoid a) => Int -> a -> a
mtimes' n x = case compare n 0 of
  LT -> errorWithoutStackTrace "AtCoder.Extra.Math.mtimes': non-negative multiplier expected"
  EQ -> mempty
  GT -> power (<>) n x
