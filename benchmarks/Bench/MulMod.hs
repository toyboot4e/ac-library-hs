module Bench.MulMod (benches) where

import BenchLib.MulMod.Barrett64 qualified as Barrett64
import BenchLib.MulMod.BarrettWideWord qualified as BarrettWideWord
import BenchLib.MulMod.Mongomery qualified as Mongomery
import Criterion
import Data.Vector.Unboxed qualified as VU
import Data.Word (Word32, Word64)
import System.Random

benches :: Benchmark
benches =
  bgroup
    "mulMod"
    [ bench "barrett32WideWord" $ whnf (VU.foldl' (BarrettWideWord.mulMod btWW) 2) randomVec32,
      bench "mod32" $ whnf (VU.foldl' (\acc n -> acc * n `mod` 998244353) 2) randomVec32,
      bench "rem32" $ whnf (VU.foldl' (\acc n -> acc * n `rem` 998244353) 2) randomVec32,
      bench "barrett64" $ whnf (VU.foldl' (Barrett64.mulMod bt64) 2) randomVec64,
      bench "mongomery" $ whnf (VU.foldl' (Mongomery.mulMod mongo) 2) randomVec64,
      bench "mod64" $ whnf (VU.foldl' (\acc n -> acc * n `mod` 998244353) 2) randomVec64,
      bench "rem64" $ whnf (VU.foldl' (\acc n -> acc * n `rem` 998244353) 2) randomVec64
    ]
  where
    n = 10000
    bt64 = Barrett64.new 998244353
    btWW = BarrettWideWord.new 998244353
    mongo = Mongomery.new 998244353
    randomVec32 :: VU.Vector Word32
    randomVec32 = VU.map fromIntegral $ VU.unfoldrExactN n (genWord64R (998244383 - 1)) (mkStdGen 123456789)
    randomVec64 :: VU.Vector Word64
    randomVec64 = VU.unfoldrExactN n (genWord64R (998244383 - 1)) (mkStdGen 123456789)

