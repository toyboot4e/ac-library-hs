module Bench.Matrix (benches) where

import AtCoder.ModInt qualified as M
import AtCoder.Extra.Semigroup.Matrix qualified as ACMAT
import AtCoder.Extra.Math qualified as ACEM
import BenchLib.Matrix qualified as Mat
import Control.Monad.State.Class (MonadState, state)
import Control.Monad.Trans.State.Strict (evalState, runState)
import Criterion
import Data.Semigroup (Semigroup (..))
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import System.Random

testSize :: Int
testSize = 10000

m :: Int
m = 998244353

randomMatrix :: (MonadState StdGen m) => m (Mat.Matrix Int, Int)
randomMatrix = do
  h <- state $ uniformR (1, 16)
  nextMatrix h

nextMatrix :: (MonadState StdGen m) => Int -> m (Mat.Matrix Int, Int)
nextMatrix h = do
  w <- state $ uniformR (1, 16)
  vec <- VU.replicateM (h * w) $ state (uniformR (0, m - 1))
  pure (Mat.new h w vec, w)

randomSquareACLMatrix :: (MonadState StdGen m) => Int -> m (ACMAT.Matrix Int)
randomSquareACLMatrix n = do
  vec <- VU.replicateM (n * n) $ state (uniformR (0, m - 1))
  pure $ ACMAT.new n n vec

benches :: Benchmark
benches =
  bgroup
    "Matrix"
    [ bench "mul1" $ whnf (V.foldl1' Mat.mul1) randomMatrixInput,
      bench "mul2" $ whnf (V.foldl1' Mat.mul2) randomMatrixInput,
      bench "mul3_1" $ whnf (V.foldl1' Mat.mul3_1) randomMatrixInput,
      bench "mul3_2" $ whnf (V.foldl1' Mat.mul3_2) randomMatrixInput,
      bench "mul3_3" $ whnf (V.foldl1' Mat.mul3_3) randomMatrixInput,
      bench "mulMod1" $ whnf (V.foldl1' (Mat.mulMod1 m)) randomMatrixInput,
      bench "mulMod2" $ whnf (V.foldl1' (Mat.mulMod2 m)) randomMatrixInput,
      bench "mulMod3" $ whnf (V.foldl1' (Mat.mulMod3 m)) randomMatrixInput,
      bench "mulMod4" $ whnf (V.foldl1' (Mat.mulMod4 m)) randomMatrixInput,
      bench "mulMod5" $ whnf (V.foldl1' (Mat.mulMod5 m)) randomMatrixInput,
      bench "mulModMint" $ whnf (V.foldl1' Mat.mulModMint1) randomMintMatrixInput,
      bench "mulModMint" $ whnf (V.foldl1' Mat.mulModMint2) randomMintMatrixInput,
      bench "mulModMint" $ whnf (V.foldl1' Mat.mulModMint3) randomMintMatrixInput,
      bench "powMod_ACL" $ whnf (VU.foldl' (flip (ACMAT.powMod 998244353)) squareMat) randomVec,
      bench "powModMintACL_stimes" $ whnf (VU.foldl' (flip stimes) squareMatMint) randomVec,
      bench "powModMintACL_stimes'" $ whnf (VU.foldl' (flip ACEM.stimes') squareMatMint) randomVec,
      bench "powModMintACL_powModMint" $ whnf (VU.foldl' (flip ACMAT.powModMint) squareMatMint) randomVec
    ]
  where
    -- Bench matrix
    randomMatrixInput :: V.Vector (Mat.Matrix Int)
    randomMatrixInput = evalState (V.unfoldrExactNM testSize nextMatrix (Mat.wM mat0)) gen0
      where
        ((!mat0, !_), !gen0) = runState randomMatrix $ mkStdGen 123456789

    randomMintMatrixInput :: V.Vector (Mat.Matrix (M.ModInt 998244353))
    randomMintMatrixInput = V.map (Mat.map M.new) randomMatrixInput

    -- ACL matrix
    squareMat :: ACMAT.Matrix Int
    squareMat = evalState (randomSquareACLMatrix 17) $ mkStdGen 123456789

    squareMatMint ::ACMAT.Matrix (M.ModInt 998244353)
    squareMatMint = ACMAT.map M.new squareMat

    -- non-zero random vector
    randomVec :: VU.Vector Int
    randomVec =
      VU.map ((+ 1) . fromIntegral) $
        VU.unfoldrExactN 100 (genWord64R (998244383 - 2)) (mkStdGen 123456789)
