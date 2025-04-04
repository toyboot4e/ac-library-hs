module Bench.SwapDupe (benches) where

import AtCoder.Extra.Graph qualified as Gr
import BenchLib.SwapDupe qualified as SwapDupe
import Criterion
import Data.Vector.Unboxed qualified as VU
import System.Random

benches :: Benchmark
benches =
  bgroup
    "build . swapDupe"
    [ bench "concatMap" $ whnf (Gr.build n . SwapDupe.swapDupeConcatMap) r,
      bench "++" $ whnf (Gr.build n . SwapDupe.swapDupePP) r,
      bench "create" $ whnf (Gr.build n . SwapDupe.swapDupeST) r
    ]
  where
    n = 10 ^ 6 :: Int
    r1, r2, r3 :: VU.Vector Int
    r1 = VU.unfoldrExactN n (uniformR (0, n - 1)) (mkStdGen (1 + 123456789))
    r2 = VU.unfoldrExactN n (uniformR (0, n - 1)) (mkStdGen (2 + 123456789))
    r3 = VU.unfoldrExactN n (uniformR (0, n - 1)) (mkStdGen (3 + 123456789))
    r = VU.zip3 r1 r2 r3
