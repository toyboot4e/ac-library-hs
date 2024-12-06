-- | Extra math module.
module AtCoder.Extra.Math
  ( power,
  )
where

import Data.Bits ((.>>.))

-- | \(O(W)\) Calculates @s^n@ by @n@ (N > 0) times using the binary lifting technique.
{-# INLINE power #-}
power :: Int -> (a -> a -> a) -> a -> a
power n0 op x1
  | n0 <= 0 = errorWithoutStackTrace "AtCoder.Extra.power: positive multiplier expected"
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


