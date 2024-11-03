module AtCoder.Internal.Bit (bitCeil) where

-- TODO: faster implmentation

bitCeil :: Int -> Int
bitCeil n = inner 1
  where
    inner x
      | x >= n = x
      | otherwise = inner $ 2 * x

