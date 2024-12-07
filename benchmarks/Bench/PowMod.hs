module Bench.PowMod (benches) where

import BenchLib.PowMod qualified as PowMod
import Criterion
import Data.Vector.Unboxed qualified as VU
import System.Random

benches :: Benchmark
benches =
  bgroup
    "powMod"
    [ bench "barrett" $ whnf (VU.foldl' (\acc n -> PowMod.powModBT acc n 998244353) 2) randomVec,
      bench "mod" $ whnf (VU.foldl' (\acc n -> PowMod.powModMod acc n 998244353) 2) randomVec,
      bench "rem" $ whnf (VU.foldl' (\acc n -> PowMod.powModRem acc n 998244353) 2) randomVec,
      bench "powerMod" $ whnf (VU.foldl' (\acc n -> PowMod.powModPowerMod acc n 998244353) 2) randomVec,
      bench "powerRem" $ whnf (VU.foldl' (\acc n -> PowMod.powModPowerRem acc n 998244353) 2) randomVec
    ]
  where
    n = 10000
    randomVec :: VU.Vector Int
    randomVec =
      VU.map fromIntegral $
        VU.unfoldrExactN n (genWord64R (998244383 - 1)) (mkStdGen 123456789)
