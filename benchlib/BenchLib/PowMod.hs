module BenchLib.PowMod
  ( powModBT,
    powModMod,
    powModRem,
    powModPowerMod,
    powModPowerRem,
  )
where

import AtCoder.Extra.Math qualified as ACEM
import AtCoder.Internal.Assert qualified as ACIA
import AtCoder.Internal.Barrett qualified as ACIBT
import Data.Bits ((.>>.))
import Data.Word (Word32)
import GHC.Stack (HasCallStack)

-- safeMod :: Int -> Int -> Int
-- safeMod = mod

-- | Barrett-based
powModBT :: (HasCallStack) => Int -> Int -> Int -> Int
powModBT x n0 m0
  | m0 == 1 = 0
  | otherwise = fromIntegral $ inner n0 1 $ fromIntegral (x `mod` m0)
  where
    !_ = ACIA.runtimeAssert (0 <= n0 && 1 <= m0) $ "BenchLib.PowMod.powModBT: given invalid `n` or `m`: " ++ show (n0, m0)
    bt = ACIBT.new $ fromIntegral m0
    inner :: Int -> Word32 -> Word32 -> Word32
    inner !n !r !y
      | n == 0 = r
      | otherwise =
          let r' = if odd n then ACIBT.mul bt r y else r
              y' = ACIBT.mul bt y y
           in inner (n .>>. 1) r' y'

-- | mod-based
powModMod :: (HasCallStack) => Int -> Int -> Int -> Int
powModMod x n0 m0
  | m0 == 1 = 0
  | otherwise = inner n0 1 $ fromIntegral (x `mod` m0)
  where
    !_ = ACIA.runtimeAssert (0 <= n0 && 1 <= m0) $ "BenchLib.PowMod.powModMod: given invalid `n` or `m`: " ++ show (n0, m0)
    inner :: Int -> Int -> Int -> Int
    inner !n !r !y
      | n == 0 = r
      | otherwise =
          let r' = if odd n then r * y `mod` m0 else r
              y' = y * y `mod` m0
           in inner (n .>>. 1) r' y'

-- | rem-based. Be warned
powModRem :: (HasCallStack) => Int -> Int -> Int -> Int
powModRem x n0 m0
  | m0 == 1 = 0
  | otherwise = inner n0 1 $ fromIntegral (x `mod` m0)
  where
    !_ = ACIA.runtimeAssert (0 <= n0 && 1 <= m0) $ "BenchLib.PowMod.powModRem: given invalid `n` or `m`: " ++ show (n0, m0)
    inner :: Int -> Int -> Int -> Int
    inner !n !r !y
      | n == 0 = r
      | otherwise =
          let r' = if odd n then r * y `rem` m0 else r
              y' = y * y `rem` m0
           in inner (n .>>. 1) r' y'

-- | power-based
powModPowerMod :: (HasCallStack) => Int -> Int -> Int -> Int
powModPowerMod x n0 m0
  | m0 == 1 = 0
  | otherwise = ACEM.power n0 (\ !a !b -> a * b `mod` m0) x

-- | power-based
powModPowerRem :: (HasCallStack) => Int -> Int -> Int -> Int
powModPowerRem x n0 m0
  | m0 == 1 = 0
  | otherwise = ACEM.power n0 (\ !a !b -> a * b `rem` m0) x
