module Bench.PowMod (benches) where

import BenchLib.PowMod qualified as PowMod
import Criterion
import Criterion.Main
import Data.Proxy
import Data.Vector.Unboxed qualified as VU
import Data.Word
import System.Random

benches :: Benchmark
benches =
  bgroup
    "powMod"
    [ bench "barrett" $ whnf (VU.foldl' (\acc n -> PowMod.powModBT acc n 998244353) 2) randoms,
      bench "mod" $ whnf (VU.foldl' (\acc n -> PowMod.powModMod acc n 998244353) 2) randoms,
      bench "rem" $ whnf (VU.foldl' (\acc n -> PowMod.powModRem acc n 998244353) 2) randoms
    ]
  where
    n = 10000
    randoms :: VU.Vector Int
    randoms =
      VU.map fromIntegral $
        VU.unfoldrExactN n (genWord64R (998244383 - 1)) (mkStdGen 123456789)
