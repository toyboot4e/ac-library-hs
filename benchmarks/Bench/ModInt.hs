module Bench.ModInt (benches) where

import AtCoder.ModInt qualified as M
import BenchLib.ModInt.ModIntNats qualified as MN
import BenchLib.PowMod qualified as PowMod
import Criterion
import Data.Vector.Unboxed qualified as VU
import System.Random

benches :: Benchmark
benches =
  bgroup
    "modInt"
    []
  where
    n = 10000
    randomVec :: VU.Vector Int
    randomVec =
      VU.map fromIntegral $
        VU.unfoldrExactN n (genWord64R (998244383 - 2)) (mkStdGen 123456789)
