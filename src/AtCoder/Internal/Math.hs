module AtCoder.Internal.Math (floorSumUnsigned) where

import Control.Exception (assert)

-- | \(O(\log m)\)
--
-- = Input constraints
-- - \(n \lt 2^32\)
-- - \(1 \le m \lt 2^32\)
floorSumUnsigned :: Int -> Int -> Int -> Int -> Int
floorSumUnsigned n0 m0 a0 b0 = inner 0 n0 m0 a0 b0
  where
    inner acc n m a b
      | yMax < m = acc'
      | otherwise = inner acc' (yMax `div` m) a' m (yMax `rem` m)
      where
        a'
          | a >= m = a `rem` m
          | otherwise = a
        b'
          | b >= m = b `rem` m
          | otherwise = b
        da
          | a >= m = n * (n - 1) `div` 2 * (a `div` m)
          | otherwise = 0
        db
          | b >= m = n * (b `div` m)
          | otherwise = 0
        acc' = acc + da + db
        yMax = a' * n + b'
