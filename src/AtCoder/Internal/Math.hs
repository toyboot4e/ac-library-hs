module AtCoder.Internal.Math (floorSumUnsigned) where

-- | \(O(\log m)\)
--
-- = Constraints
-- - \(n \lt 2^32\)
-- - \(1 \le m \lt 2^32\)
floorSumUnsigned :: Int -> Int -> Int -> Int -> Int
floorSumUnsigned = inner 0
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
