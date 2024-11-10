{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module AtCoder.Math (floorSum) where

import AtCoder.Internal.Assert qualified as ACIA
import AtCoder.Internal.Math qualified as ACIM
import Data.Bits (bit)
import GHC.Stack (HasCallStack)

-- TODO: check if it's also the case with Haskell port:
-- > It returns the answer in $\bmod 2^{\mathrm{64}}$, if overflowed.

-- | returns \(\sum\limits_{i = 0}^{n - 1} \left\lfloor \frac{a \times i + b}{m} \right\rfloor\)
--
-- = Constraints
-- - \(0 \le n \lt 2^32\)
-- - \(1 \le m \lt 2^32\)
--
-- = Complexity
-- - \(O(\log m)\)
floorSum :: (HasCallStack) => Int -> Int -> Int -> Int -> Int
floorSum n m a b = ACIM.floorSumUnsigned n m a' b' - da - db
  where
    !_ = ACIA.runtimeAssert (0 <= n && n < bit 32) $ "AtCoder.Math.floorSum: given invalid `n` (`" ++ show n ++ "`)"
    !_ = ACIA.runtimeAssert (1 <= m && m < bit 32) $ "AtCoder.Math.floorSum: given invalid `m` (`" ++ show m ++ "`)"
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
