-- | Benchmark for monadic streams.
module Bench.RepeatWithoutIndex (benches) where

import Criterion
import Control.Monad (when, replicateM_)
import Control.Monad.ST (runST)
import Data.Foldable (for_)
import Data.Vector.Fusion.Stream.Monadic qualified as MS
import Data.Vector.Generic.Mutable as VGM
import Data.Vector.Unboxed as VU
import Data.Vector.Unboxed.Mutable as VUM

len :: Int
len = 10 ^ 7

list :: Int -> Int
list x = runST $ do
  res <- VUM.replicate 1 x
  for_ [0 .. len - 1] $ \_ -> do
    VGM.modify res (+ 1) 0
  VGM.read res 0

-- | @cojna/iota
(..<) :: (Monad m) => Int -> Int -> MS.Stream m Int
(..<) !l !r = MS.Stream step l
  where
    step x
      | x < r = return $ MS.Yield x (x + 1)
      | otherwise = return MS.Done
    {-# INLINE [0] step #-}
{-# INLINE [1] (..<) #-}

stream :: Int -> Int
stream x = runST $ do
  res <- VUM.replicate 1 x
  flip MS.mapM_ (0 ..< len) $ \_ -> do
    VGM.modify res (+ 1) 0
  VGM.read res 0

vector :: Int -> Int
vector x = runST $ do
  res <- VUM.replicate 1 x
  VU.forM_ (VU.generate len id) $ \_ -> do
    VGM.modify res (+ 1) 0
  VGM.read res 0

recursion :: Int -> Int
recursion x = runST $ do
  res <- VUM.replicate 1 x
  let run i = do
        when (i < len) $ do
          VGM.modify res (+ 1) 0
          run $ i + 1
  run 0
  VGM.read res 0

rep :: Int -> Int
rep x = runST $ do
  res <- VUM.replicate 1 x
  replicateM_ len $ do
    VGM.modify res (+ 1) 0
  VGM.read res 0

-- The result is suspecious.. the stream version is slower than vector, recursive or replicateM_ is
-- faster by 10 times?
benches :: Benchmark
benches =
  bgroup
    "repeat-without-index"
    [ bench "list" $ nf list 0,
      bench "stream" $ nf stream 0,
      bench "vector" $ nf vector 0,
      bench "recursion" $ nf recursion 0,
      bench "rep" $ nf rep 0
    ]
