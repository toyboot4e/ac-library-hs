module BenchLib.PowMod
  ( powModBarrettWideWord,
    powModBarrett64,
    powModMontgomery,
    powModMod,
    powModRem,
    powModPowerMod,
    powModPowerRem,
  )
where

import AtCoder.Extra.Math qualified as ACEM
import AtCoder.Internal.Assert qualified as ACIA
import BenchLib.MulMod.Barrett64 qualified as Barrett64
import BenchLib.MulMod.BarrettWideWord qualified as BarrettWideWord
import BenchLib.MulMod.Montgomery qualified as Montgomery
import Data.Bits ((.>>.))
import Data.Word (Word64)
import GHC.Stack (HasCallStack)

-- safeMod :: Int -> Int -> Int
-- safeMod = mod

-- TODO: run unit test. probablly wrong.

-- | Wide word Barrett-based
powModBarrettWideWord :: (HasCallStack) => Int -> Int -> Int -> Int
powModBarrettWideWord x n0 m0
  | m0 == 1 = 0
  | otherwise = fromIntegral $ inner n0 1 $ fromIntegral (x `mod` m0)
  where
    !_ = ACIA.runtimeAssert (0 <= n0 && 1 <= m0) $ "BenchLib.PowMod.powModBarrettWideWord: given invalid `n` or `m`: " ++ show (n0, m0)
    bt = BarrettWideWord.new64 $ fromIntegral m0
    inner :: Int -> Word64 -> Word64 -> Word64
    inner !n !r !y
      | n == 0 = r
      | otherwise =
          let r' = if odd n then BarrettWideWord.mulMod bt r y else r
              y' = BarrettWideWord.mulMod bt y y
           in inner (n .>>. 1) r' y'

-- | Barrett64-based
powModBarrett64 :: (HasCallStack) => Int -> Int -> Int -> Int
powModBarrett64 x n0 m0
  | m0 == 1 = 0
  | otherwise = fromIntegral $ inner n0 1 $ fromIntegral (x `mod` m0)
  where
    !_ = ACIA.runtimeAssert (0 <= n0 && 1 <= m0) $ "BenchLib.PowMod.powModBarrett64: given invalid `n` or `m`: " ++ show (n0, m0)
    bt = Barrett64.new $ fromIntegral m0
    inner :: Int -> Word64 -> Word64 -> Word64
    inner !n !r !y
      | n == 0 = r
      | otherwise =
          let r' = if odd n then Barrett64.mulMod bt r y else r
              y' = Barrett64.mulMod bt y y
           in inner (n .>>. 1) r' y'

-- | Montgomery-based
powModMontgomery :: (HasCallStack) => Int -> Int -> Int -> Int
powModMontgomery x n0 m0
  | m0 == 1 = 0
  | otherwise = fromIntegral . Montgomery.reduce mont $ inner n0 (Montgomery.generate mont 1) . Montgomery.generate mont $ fromIntegral (x `mod` m0)
  where
    !_ = ACIA.runtimeAssert (0 <= n0 && 1 <= m0) $ "BenchLib.PowMod.powModMontgomery: given invalid `n` or `m`: " ++ show (n0, m0)
    mont = Montgomery.new $ fromIntegral m0
    inner :: Int -> Word64 -> Word64 -> Word64
    inner !n !r !y
      | n == 0 = r
      | otherwise =
          let r' = if odd n then Montgomery.mulModGenerated mont r y else r
              y' = Montgomery.mulModGenerated mont y y
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
