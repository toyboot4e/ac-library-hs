module Bench.PowMod (benches) where

import BenchLib.PowMod qualified as PowMod
import Criterion
import Data.Vector.Unboxed qualified as VU
import System.Random

benches :: Benchmark
benches =
  bgroup
    "powMod"
    -- TODO: change their signature
    -- TODO: unit test
    [ bench "barrettWideWord" $ whnf (VU.foldl' (\acc x -> PowMod.powModBarrettWideWord acc x 998244353) 2) randomVec,
      bench "barrett64" $ whnf (VU.foldl' (\acc x -> PowMod.powModBarrett64 acc x 998244353) 2) randomVec,
      bench "montgomery" $ whnf (VU.foldl' (\acc x -> PowMod.powModMontgomery acc x 998244353) 2) randomVec,
      bench "mod" $ whnf (VU.foldl' (\acc x -> PowMod.powModMod acc x 998244353) 2) randomVec,
      bench "rem" $ whnf (VU.foldl' (\acc x -> PowMod.powModRem acc x 998244353) 2) randomVec,
      bench "powerMod" $ whnf (VU.foldl' (\acc x -> PowMod.powModPowerMod acc x 998244353) 2) randomVec,
      bench "powerRem" $ whnf (VU.foldl' (\acc x -> PowMod.powModPowerRem acc x 998244353) 2) randomVec
    ]
  where
    n = 10000
    randomVec :: VU.Vector Int
    randomVec =
      VU.map fromIntegral $
        VU.unfoldrExactN n (genWord64R (998244383 - 2)) (mkStdGen 123456789)
