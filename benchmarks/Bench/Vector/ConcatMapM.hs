module Bench.Vector.ConcatMapM (benches) where

import BenchLib.Vector.ConcatMapM qualified as ConcatMapM
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.ST (runST)
import Control.Monad.State.Class (MonadState, modify')
import Control.Monad.Trans.State.Strict (StateT (..), evalState, evalStateT)
import Criterion
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import System.Random

benches :: Benchmark
benches =
  bgroup
    "concatMapM"
    [ -- FIXME: too fast, something is wrong.
      bench "list" $ nf (concatMap fList) (VU.toList vec),
      bench "prim    IO" $ nfIO (prim1 vec),
      bench "monad 1 IO" $ nfIO (io1 vec),
      bench "monad 2 IO" $ nfIO (io2 vec),
      bench "prim    ST" $ nf (\xs -> runST (prim1 xs)) vec,
      bench "monad 1 ST" $ nf st1 vec,
      bench "monad 2 ST" $ nf st2 vec,
      bench "prim    StateT" $ nf (\vec -> runST $ (`evalStateT` (0 :: Int)) (ConcatMapM.primConcatMapM1 g vec)) vec,
      bench "monad 1 State" $ nf ((`evalState` (0 :: Int)) . ConcatMapM.concatMapM1 g) vec,
      bench "monad 2 State" $ nf ((`evalState` (0 :: Int)) . ConcatMapM.concatMapM2 g) vec,
      bench "prime   StateT + IO" $ nfIO (evalStateT (primStateT1 vec) (0 :: Int)),
      bench "monad 1 StateT + IO" $ nfIO (evalStateT (stateT1 vec) (0 :: Int)),
      bench "monad 2 StateT + IO" $ nfIO (evalStateT (stateT2 vec) (0 :: Int))
    ]
  where
    n = 10 ^ 3 :: Int
    vec :: VU.Vector Int
    vec = VU.unfoldrExactN n (uniformR (0, n - 1)) (mkStdGen (1 + 123456789))

    f :: Int -> VU.Vector Int
    f x = VU.fromList [x, x, x, x, x]

    fList :: Int -> [Int]
    fList x = [x, x, x, x, x]

    g :: (MonadState Int m) => Int -> m (VU.Vector Int)
    g x = do
      modify' (+ 1)
      pure $ f x

    prim1 :: (PrimMonad m) => VU.Vector Int -> m (VU.Vector Int)
    prim1 vec = do
      ref <- VUM.replicate 1 (0 :: Int)
      ConcatMapM.primConcatMapM1
        ( \x -> do
            VUM.modify ref (+ 1) 0
            pure $ f x
        )
        vec

    io1 :: VU.Vector Int -> IO (VU.Vector Int)
    io1 vec = do
      ref <- VUM.replicate 1 (0 :: Int)
      ConcatMapM.concatMapM1
        ( \x -> do
            VUM.modify ref (+ 1) 0
            pure $ f x
        )
        vec

    io2 :: VU.Vector Int -> IO (VU.Vector Int)
    io2 vec = do
      ref <- VUM.replicate 1 (0 :: Int)
      ConcatMapM.concatMapM2
        ( \x -> do
            VUM.modify ref (+ 1) 0
            pure $ f x
        )
        vec

    st1 :: VU.Vector Int -> VU.Vector Int
    st1 vec = runST $ do
      ref <- VUM.replicate 1 (0 :: Int)
      ConcatMapM.concatMapM1
        ( \x -> do
            VUM.modify ref (+ 1) 0
            pure $ f x
        )
        vec

    st2 :: VU.Vector Int -> VU.Vector Int
    st2 vec = runST $ do
      ref <- VUM.replicate 1 (0 :: Int)
      ConcatMapM.concatMapM2
        ( \x -> do
            VUM.modify ref (+ 1) 0
            pure $ f x
        )
        vec

    stateT1 :: VU.Vector Int -> StateT Int IO (VU.Vector Int)
    stateT1 vec = do
      ref <- VUM.replicate 1 (0 :: Int)
      ConcatMapM.concatMapM1
        ( \x -> do
            VUM.modify ref (+ 1) 0
            modify' (+ 1)
            pure $ f x
        )
        vec

    stateT2 :: VU.Vector Int -> StateT Int IO (VU.Vector Int)
    stateT2 vec = do
      ref <- VUM.replicate 1 (0 :: Int)
      ConcatMapM.concatMapM2
        ( \x -> do
            VUM.modify ref (+ 1) 0
            modify' (+ 1)
            pure $ f x
        )
        vec

    primStateT1 :: VU.Vector Int -> StateT Int IO (VU.Vector Int)
    primStateT1 vec = do
      ref <- VUM.replicate 1 (0 :: Int)
      ConcatMapM.primConcatMapM1
        ( \x -> do
            VUM.modify ref (+ 1) 0
            modify' (+ 1)
            pure $ f x
        )
        vec
