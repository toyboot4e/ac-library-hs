module Bench.MulMod (benches32, benches64) where

import BenchLib.MulMod.Barrett64 qualified as Barrett64
import BenchLib.MulMod.BarrettWideWord qualified as BarrettWideWord
import BenchLib.MulMod.Montgomery qualified as Montgomery
import Criterion
import Data.Bifunctor (first)
import Data.Vector.Unboxed qualified as VU
import Data.Word (Word32, Word64)
import System.Random

n :: Int
n = 10000

btWW :: BarrettWideWord.Barrett
btWW = BarrettWideWord.new64 998244353

bt64 :: Barrett64.Barrett
bt64 = Barrett64.new 998244353

mont :: Montgomery.Montgomery
mont = Montgomery.new 998244353

-- | This benchmark is almost nonsense, as the pre calculation' overhead is not considered while the
-- real use case is @powMod@. However, it's useful when optimizing the successive calculation.
benches32 :: Benchmark
benches32 =
  bgroup
    "mulMod Word32 vector"
    [ bench "barrettWideWord" $ whnf (VU.foldl' (\acc -> BarrettWideWord.mulMod btWW acc . fromIntegral) w64) nonZeroRandomVec32,
      bench "barrett" $ whnf (VU.foldl' (\acc -> Barrett64.mulMod bt64 acc . fromIntegral) w64) nonZeroRandomVec32,
      -- NOTE: It skips the last reduce.
      bench "montgomery" $ whnf (VU.foldl' (\acc -> Montgomery.mulModGenerated mont acc . Montgomery.generate mont . fromIntegral) w64) nonZeroRandomVec32,
      bench "mod" $ whnf (VU.foldl' (\acc n -> acc * n `mod` 998244353) w32) nonZeroRandomVec32,
      bench "rem" $ whnf (VU.foldl' (\acc n -> acc * n `rem` 998244353) w32) nonZeroRandomVec32
    ]
  where
    w32 :: Word32
    w32 = 1
    w64 :: Word64
    w64 = 1
    -- [1, 998244383)
    nonZeroRandomVec32 :: VU.Vector Word32
    nonZeroRandomVec32 = VU.map fromIntegral $ VU.unfoldrExactN n (first (+ 1) . genWord64R (998244383 - 2)) (mkStdGen 123456789)

-- | This benchmark is almost nonsense, as the pre calculation' overhead is not considered while the
-- real use case is @powMod@. However, it's useful when optimizing the successive calculation.
benches64 :: Benchmark
benches64 =
  bgroup
    "mulMod Word64 vector"
    [ bench "barrettWideWord" $ whnf (VU.foldl' (BarrettWideWord.mulMod btWW) w64) nonZeroRandomVec64,
      bench "barrett" $ whnf (VU.foldl' (\acc -> Barrett64.mulMod bt64 acc) w64) nonZeroRandomVec64,
      -- NOTE: It skips the last reduce.
      bench "montgomery" $ whnf (VU.foldl' (\acc -> Montgomery.mulModGenerated mont acc . Montgomery.generate mont) w64) nonZeroRandomVec64,
      bench "mod" $ whnf (VU.foldl' (\acc n -> acc * n `mod` 998244353) w64) nonZeroRandomVec64,
      bench "rem" $ whnf (VU.foldl' (\acc n -> acc * n `rem` 998244353) w64) nonZeroRandomVec64
    ]
  where
    w64 :: Word64
    w64 = 1
    -- [1, 998244383)
    nonZeroRandomVec64 :: VU.Vector Word64
    nonZeroRandomVec64 = VU.map fromIntegral $ VU.unfoldrExactN n (first (+ 1) . genWord64R (998244383 - 2)) (mkStdGen 123456789)
