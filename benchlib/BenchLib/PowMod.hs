module Bench.PowMod (powModBT, powModMod, powModRem) where

import AtCoder.Internal.Assert qualified as ACIA
import AtCoder.Internal.Barrett qualified as ACIBT
import Control.Monad.ST (runST)
import Data.Bits ((.<<.), (.>>.))
import Data.Foldable
import Data.Maybe (fromJust)
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
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
    !_ = ACIA.runtimeAssert (0 <= n0 && 1 <= m0) $ "AtCoder.Internal.Math.powMod: given invalid `n` or `m`: " ++ show (n0, m0)
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
  | otherwise = fromIntegral $ inner n0 1 $ fromIntegral (x `mod` m0)
  where
    !_ = ACIA.runtimeAssert (0 <= n0 && 1 <= m0) $ "AtCoder.Internal.Math.powMod: given invalid `n` or `m`: " ++ show (n0, m0)
    !m :: Word32 = fromIntegral m0
    inner :: Int -> Word32 -> Word32 -> Word32
    inner !n !r !y
      | n == 0 = r
      | otherwise =
          let r' = if odd n then r * y `mod` m else r
              y' = y * y `mod` m
           in inner (n .>>. 1) r' y'

-- | rem-based. Be warned
powModRem :: (HasCallStack) => Int -> Int -> Int -> Int
powModRem x n0 m0
  | m0 == 1 = 0
  | otherwise = fromIntegral $ inner n0 1 $ fromIntegral (x `mod` m0)
  where
    !_ = ACIA.runtimeAssert (0 <= n0 && 1 <= m0) $ "AtCoder.Internal.Math.powMod: given invalid `n` or `m`: " ++ show (n0, m0)
    !m :: Word32 = fromIntegral m0
    inner :: Int -> Word32 -> Word32 -> Word32
    inner !n !r !y
      | n == 0 = r
      | otherwise =
          let r' = if odd n then r * y `rem` m else r
              y' = y * y `rem` m
           in inner (n .>>. 1) r' y'
