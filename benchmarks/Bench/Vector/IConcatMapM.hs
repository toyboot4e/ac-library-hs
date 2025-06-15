module Bench.Vector.IConcatMapM (benches) where

import BenchLib.Vector.ConcatMapM qualified as ConcatMapM
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.ST (runST)
import Control.Monad.Trans.State.Strict (State (..), StateT (..), evalState, evalStateT)
import Control.Monad.State.Class (MonadState, modify')
import Criterion
import Data.Vector.Fusion.Bundle.Monadic qualified as BundleM
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import System.Random

benches :: Benchmark
benches =
  bgroup
    "iconcatMapM"
    [ bench "prim    IO" $ nfIO (prim1 vec),
      bench "monad 1 IO" $ nfIO (io1 vec),
      bench "monad 2 IO" $ nfIO (io2 vec),
      bench "prim    ST" $ whnf (\xs -> runST (prim1 xs)) vec,
      bench "monad 1 ST" $ whnf st1 vec,
      bench "monad 2 ST" $ whnf st2 vec,
      bench "prim    StateT" $ whnf (\vec -> runST $ (`evalStateT` (0 :: Int)) (ConcatMapM.primIConcatMapM1 g vec)) vec,
      bench "monad 1 State" $ whnf ((`evalState` (0 :: Int)) . ConcatMapM.iconcatMapM1 g) vec,
      bench "monad 2 State" $ whnf ((`evalState` (0 :: Int)) . ConcatMapM.iconcatMapM2 g) vec,
      bench "prime   StateT + IO" $ nfIO (evalStateT (primStateT1 vec) (0 :: Int)),
      bench "monad 1 StateT + IO" $ nfIO (evalStateT (stateT1 vec) (0 :: Int)),
      bench "monad 2 StateT + IO" $ nfIO (evalStateT (stateT2 vec) (0 :: Int))
    ]
  where
    n = 10 ^ 3 :: Int
    vec :: VU.Vector Int
    vec = VU.unfoldrExactN n (uniformR (0, n - 1)) (mkStdGen (1 + 123456789))

    f :: Int -> Int -> VU.Vector Int
    f i x = VU.fromList [i + x, i + x, i + x, i + x, i + x]

    g :: (MonadState Int m) => Int -> Int -> m (VU.Vector Int)
    g i x = do
      modify' (+ i)
      pure $ f i x

    prim1 :: (PrimMonad m) => VU.Vector Int -> m (VU.Vector Int)
    prim1 vec = do
      ref <- VUM.replicate 1 (0 :: Int)
      ConcatMapM.primIConcatMapM1
        ( \i x -> do
            VUM.modify ref (+ i) 0
            pure $ f i x
        )
        vec

    io1 :: VU.Vector Int -> IO (VU.Vector Int)
    io1 vec = do
      ref <- VUM.replicate 1 (0 :: Int)
      ConcatMapM.iconcatMapM1
        ( \i x -> do
            VUM.modify ref (+ i) 0
            pure $ f i x
        )
        vec

    io2 :: VU.Vector Int -> IO (VU.Vector Int)
    io2 vec = do
      ref <- VUM.replicate 1 (0 :: Int)
      ConcatMapM.iconcatMapM2
        ( \i x -> do
            VUM.modify ref (+ i) 0
            pure $ f i x
        )
        vec

    st1 :: VU.Vector Int -> VU.Vector Int
    st1 vec = runST $ do
      ref <- VUM.replicate 1 (0 :: Int)
      ConcatMapM.iconcatMapM1
        ( \i x -> do
            VUM.modify ref (+ i) 0
            pure $ f i x
        )
        vec

    st2 :: VU.Vector Int -> VU.Vector Int
    st2 vec = runST $ do
      ref <- VUM.replicate 1 (0 :: Int)
      ConcatMapM.iconcatMapM2
        ( \i x -> do
            VUM.modify ref (+ i) 0
            pure $ f i x
        )
        vec

    stateT1 :: VU.Vector Int -> StateT Int IO (VU.Vector Int)
    stateT1 vec = do
      ref <- VUM.replicate 1 (0 :: Int)
      ConcatMapM.iconcatMapM1
        ( \i x -> do
            VUM.modify ref (+ i) 0
            modify' (+ i)
            pure $ f i x
        )
        vec

    stateT2 :: VU.Vector Int -> StateT Int IO (VU.Vector Int)
    stateT2 vec = do
      ref <- VUM.replicate 1 (0 :: Int)
      ConcatMapM.iconcatMapM2
        ( \i x -> do
            VUM.modify ref (+ i) 0
            modify' (+ i)
            pure $ f i x
        )
        vec

    primStateT1 :: VU.Vector Int -> StateT Int IO (VU.Vector Int)
    primStateT1 vec = do
      ref <- VUM.replicate 1 (0 :: Int)
      ConcatMapM.primIConcatMapM1
        ( \i x -> do
            VUM.modify ref (+ i) 0
            modify' (+ i)
            pure $ f i x
        )
        vec
