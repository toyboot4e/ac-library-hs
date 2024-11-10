module AtCoder.Internal.Bit (bitCeil) where

-- TODO: faster implmentation

-- | \(O(w)\) Returns minimum \(2^i s.t. 2^i \geq n\).
bitCeil :: Int -> Int
bitCeil n = inner 1
  where
    inner x
      | x >= n = x
      | otherwise = inner $ 2 * x

-- countTrailingZeros from Data.Bits

