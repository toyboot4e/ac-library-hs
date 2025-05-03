{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}

module Bench.Montgomery64 (benches) where

import BenchLib.Montgomery64.Inline qualified as M64A
import BenchLib.Montgomery64.Noinline qualified as M64B
import Criterion
import Data.Vector.Unboxed qualified as VU
import GHC.Exts (proxy#)
import System.Random

benches :: Benchmark
benches =
  bgroup
    "Montgomery64"
    [ -- The following four are actually meaningless:
      bench "new @998244353 inline" $ whnf (VU.foldl' (\ !_ !_ -> M64A.new (proxy# @998244353)) mA) randomVec,
      bench "new @998244353 noinline" $ whnf (VU.foldl' (\ !_ !_ -> M64B.new (proxy# @998244353)) mB) randomVec,
      bench "new @2305843009213693951 inline" $ whnf (VU.foldl' (\ !_ !_ -> M64A.new (proxy# @2305843009213693951)) mA) randomVec,
      bench "new @2305843009213693951 noinline" $ whnf (VU.foldl' (\ !_ !_ -> M64B.new (proxy# @2305843009213693951)) mB) randomVec,
      -- These are the important tests:
      bench "fromVal 998244353 inline" $ whnf (VU.foldl' (\ !_ !x -> M64A.fromVal (fromIntegral x)) mA) repVec1,
      bench "fromVal 998244353 noinline" $ whnf (VU.foldl' (\ !_ !x -> M64B.fromVal (fromIntegral x)) mB) repVec1,
      bench "fromVal 2305843009213693951 inline" $ whnf (VU.foldl' (\ !_ !x -> M64A.fromVal (fromIntegral x)) mA) repVec2,
      bench "fromVal 2305843009213693951 noinline" $ whnf (VU.foldl' (\ !_ !x -> M64B.fromVal (fromIntegral x)) mB) repVec2,
      bench "fromVal random inline" $ whnf (VU.foldl' (\ !_ !x -> M64A.fromVal (fromIntegral x)) mA) randomVec,
      bench "fromVal random noinline" $ whnf (VU.foldl' (\ !_ !x -> M64B.fromVal (fromIntegral x)) mB) randomVec
    ]
  where
    n = 10000
    mA = M64A.fromVal 3
    mB = M64B.fromVal 3
    randomVec :: VU.Vector Int
    randomVec =
      VU.map fromIntegral $
        VU.unfoldrExactN n ((\(!x, !gen) -> (1 + 2 * x, gen)) <$> genWord64R (2 ^ 62 `div` 2)) (mkStdGen 123456789)
    repVec1 :: VU.Vector Int
    repVec1 = VU.replicate n 998244353
    repVec2 :: VU.Vector Int
    repVec2 = VU.replicate n 2305843009213693951
