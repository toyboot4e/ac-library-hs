module Bench.Vector.MapAccumL (benches) where

import BenchLib.Vector.MapAccumL qualified as MapAccumL
import Criterion
import Data.List qualified as L
import Data.Vector.Unboxed qualified as VU
import System.Random

benches :: Benchmark
benches =
  bgroup
    "mapAccumL"
    [ -- whnf did not work (somehow) for mapAccumL1, so I'm using nf
      -- NOTE: list is not fair to compare though
      bench "list" $ nf (L.mapAccumL f (0 :: Int)) (VU.toList vec),
      bench "mapM + State" $ nf (MapAccumL.mapAccumL1 f (0 :: Int)) vec,
      bench "ifoldM'" $ nf (MapAccumL.mapAccumL2 f (0 :: Int)) vec,
      bench "bundle" $ nf (MapAccumL.mapAccumL3 f (0 :: Int)) vec,
      bench "mapM + State + PrimMonad" $ nf (MapAccumL.mapAccumL4 f (0 :: Int)) vec
    ]
  where
    n = 10 ^ 3 :: Int
    vec :: VU.Vector Int
    vec = VU.unfoldrExactN n (uniformR (0, n - 1)) (mkStdGen (1 + 123456789))
    f :: Int -> Int -> (Int, Int)
    f s x = (s + 10, s * x)
