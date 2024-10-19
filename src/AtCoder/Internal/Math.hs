module AtCoder.Internal.Math (floorSumUnsigned, invGcd) where

-- | TODO: \(O(?)\)
--
-- = Input constraints
-- - \(1 <\le b\)
--
-- Returns @(g, x)@ such that \(g = \gcd(a, b), \mathrm{xa} = g(\bmod b), 0 \le x \le b/g\)
invGcd :: Int -> Int -> (Int, Int)
invGcd a0 b
  | a == 0 = (b, 0)
  | otherwise = inner b a 0 1
  where
    a = a0 `mod` b
    -- Contracts:
    -- [1] s - m0 * a = 0 (mod b)
    -- [2] t - m1 * a = 0 (mod b)
    -- [3] s * |m1| + t * |m0| <= b
    inner !s !t !m0 !m1
      | t == 0 =
        let !m = if m0 >= 0 then m0 else m0 + b `div` s
         in (s, m)
      | otherwise = inner t s' m1 m0'
      where
        u = s `div` t
        s' = s - t * u
        m0' = m0 - m1 * u -- |m1 * u| <= |m1| * s <= b

-- | \(O(\log m)\)
--
-- = Input constraints
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
