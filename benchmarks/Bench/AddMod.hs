{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}

module Bench.AddMod (benches) where

import BenchLib.AddMod
import Criterion
import Data.Vector.Unboxed qualified as VU
import Data.Word (Word32)
import System.Random

n :: Int
n = 10000

benches :: Benchmark
benches =
  bgroup
    "addMod"
    [
      bench "addModMod" $ whnf (VU.foldl' (addModMod 998244353) w32) randomVec32,
      bench "addModRem" $ whnf (VU.foldl' (addModRem 998244353) w32) randomVec32,
      bench "addModSub" $ whnf (VU.foldl' (addModSub 998244353) w32) randomVec32,
      bench "addModRem#" $ whnf (VU.foldl' (addModRem# 998244353) w32) randomVec32,
      bench "addModSub#" $ whnf (VU.foldl' (addModSub# 998244353) w32) randomVec32
    ]
  where
    w32 :: Word32
    w32 = 1
    -- [1, 998244383)
    randomVec32 :: VU.Vector Word32
    randomVec32 = VU.map fromIntegral $ VU.unfoldrExactN n (genWord64R (998244383 - 1)) (mkStdGen 123456789)
