{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module AtCoder.Math (floorSum) where

import AtCoder.Internal.Math (floorSumUnsigned)
import Control.Exception (assert)
import Data.Bits (bit)

-- | \(O(\log m)\) Returns \(\sum_{i=0}^{j-1} \lfloor \frac {a \cross i + b} {m} \rfloor\).
--
-- = Input constraints
-- - \(0 \le n \lt 2^32\)
-- - \(1 \le m \lt 2^32\)
floorSum :: Int -> Int -> Int -> Int -> Int
floorSum n m a b = floorSumUnsigned n m a' b' - da - db
  where
    !_ = assert (0 <= n && n < bit 32) ()
    !_ = assert (1 <= m && m < bit 32) ()
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
