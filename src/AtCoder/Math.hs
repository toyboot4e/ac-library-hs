{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module AtCoder.Math (floorSum) where

import AtCoder.Internal.Assert (runtimeAssert)
import AtCoder.Internal.Math (floorSumUnsigned)
import Data.Bits (bit)
import GHC.Stack (HasCallStack)

-- | \(O(\log m)\) Returns \(\sum_{i=0}^{j-1} \lfloor \frac {a \cross i + b} {m} \rfloor\).
--
-- = Input constraints
-- - \(0 \le n \lt 2^32\)
-- - \(1 \le m \lt 2^32\)
floorSum :: (HasCallStack) => Int -> Int -> Int -> Int -> Int
floorSum n m a b = floorSumUnsigned n m a' b' - da - db
  where
    !_ = runtimeAssert (0 <= n && n < bit 32) "floorSum: invalid n"
    !_ = runtimeAssert (1 <= m && m < bit 32) "floorSum: invalid m"
    a'
      | a < 0 = a `mod` m
      | otherwise = a
    da
      | a < 0 = n * (n - 1) `div` 2 * (((a `mod` m) - a) `div` m)
      | otherwise = 0
    b'
      | b < 0 = b `mod` m
      | otherwise = b
    db
      | b < 0 = n * (((b `mod` m) - b) `div` m)
      | otherwise = 0
