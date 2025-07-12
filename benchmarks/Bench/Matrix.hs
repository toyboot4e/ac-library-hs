{-# LANGUAGE DataKinds #-}

module Bench.Matrix (benches) where

import AtCoder.Extra.Math qualified as ACEM
import AtCoder.Extra.Semigroup.Matrix qualified as ACMAT
import AtCoder.ModInt qualified as M
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
      bench "mul4_1" $ whnf (V.foldl1' Mat.mul4_1) randomMatrixInput,
      bench "mul4_2" $ whnf (V.foldl1' Mat.mul4_2) randomMatrixInput,
      bench "mul4_3" $ whnf (V.foldl1' Mat.mul4_3) randomMatrixInput,
      bench "mulMod1" $ whnf (V.foldl1' (Mat.mulMod1 m)) randomMatrixInput,
      bench "mulMod2" $ whnf (V.foldl1' (Mat.mulMod2 m)) randomMatrixInput,
      bench "mulMod3" $ whnf (V.foldl1' (Mat.mulMod3 m)) randomMatrixInput,
      bench "mulMod4" $ whnf (V.foldl1' (Mat.mulMod4 m)) randomMatrixInput,
      bench "mulMod5" $ whnf (V.foldl1' (Mat.mulMod5 m)) randomMatrixInput,
      bench "mulMod6" $ whnf (V.foldl1' (Mat.mulMod6 m)) randomMatrixInput,
      bench "mulMod7" $ whnf (V.foldl1' (Mat.mulMod7 m)) randomMatrixInput,
      bench "mulMint1" $ whnf (V.foldl1' Mat.mulMint1) randomMintMatrixInput,
      bench "mulMint2" $ whnf (V.foldl1' Mat.mulMint2) randomMintMatrixInput,
      bench "mulMint3" $ whnf (V.foldl1' Mat.mulMint3) randomMintMatrixInput,
      bench "mulMint4" $ whnf (V.foldl1' Mat.mulMint4) randomMintMatrixInput,
      -- mul (ACL)
      bench "mul_ACL" $ whnf (V.foldl1' ACMAT.mul) randomMatrixInputACL,
      bench "mulMod_ACL" $ whnf (V.foldl1' (ACMAT.mulMod m)) randomMatrixInputACL,
      bench "mulMint_ACL" $ whnf (V.foldl1' ACMAT.mulMint) randomMintMatrixInputACL,
      -- pow mod (ACL only)
      bench "powMod_ACL" $ whnf (VU.foldl' (flip (ACMAT.powMod m)) squareMat) randomVec,
      bench "powMintACL_stimes" $ whnf (VU.foldl' (flip stimes) squareMatMint) randomVec,
      bench "powMintACL_stimes'" $ whnf (VU.foldl' (flip ACEM.stimes') squareMatMint) randomVec,
      bench "powMintACL_powMint" $ whnf (VU.foldl' (flip ACMAT.powMint) squareMatMint) randomVec
    ]
  where
    -- Bench matrix
    randomMatrixInput :: V.Vector (Mat.Matrix Int)
    !randomMatrixInput = evalState (V.unfoldrExactNM testSize nextMatrix (Mat.wM mat0)) gen0
      where
        ((!mat0, !_), !gen0) = runState randomMatrix $ mkStdGen 123456789

    randomMintMatrixInput :: V.Vector (Mat.Matrix (M.ModInt 998244353))
    !randomMintMatrixInput = V.map (Mat.map M.new) randomMatrixInput

    -- ACL matrix
    randomMatrixInputACL :: V.Vector (ACMAT.Matrix Int)
    !randomMatrixInputACL = V.map (\(Mat.Matrix h w vec) -> ACMAT.new h w vec) randomMatrixInput

    randomMintMatrixInputACL :: V.Vector (ACMAT.Matrix (M.ModInt 998244353))
    !randomMintMatrixInputACL = V.map (\(Mat.Matrix h w vec) -> ACMAT.new h w vec) randomMintMatrixInput

    squareMat :: ACMAT.Matrix Int
    !squareMat = evalState (randomSquareACLMatrix 17) $ mkStdGen 123456789

    squareMatMint :: ACMAT.Matrix (M.ModInt 998244353)
    !squareMatMint = ACMAT.map M.new squareMat

    -- non-zero random vector
    randomVec :: VU.Vector Int
    !randomVec =
      VU.map ((+ 1) . fromIntegral) $
        VU.unfoldrExactN 100 (genWord64R (998244353 - 2)) (mkStdGen 123456789)
