-- | Benchmark for monadic streams.
module Bench.RepeatWithIndex (benches) where

import Control.Monad (replicateM_, when)
import Control.Monad.ST (runST)
import Criterion
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
  for_ [0 .. len - 1] $ \dx -> do
    VGM.modify res (+ dx) 0
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
  flip MS.mapM_ (0 ..< len) $ \dx -> do
    VGM.modify res (+ dx) 0
  VGM.read res 0

vector :: Int -> Int
vector x = runST $ do
  res <- VUM.replicate 1 x
  VU.forM_ (VU.generate len id) $ \dx -> do
    VGM.modify res (+ dx) 0
  VGM.read res 0

recursion :: Int -> Int
recursion x = runST $ do
  res <- VUM.replicate 1 x
  let run i = do
        when (i < len) $ do
          VGM.modify res (+ i) 0
          run $ i + 1
  run 0
  VGM.read res 0

-- The result is suspicious.. the stream version is slower than vector, recursive or replicateM_ is
-- faster by 10 times?
benches :: Benchmark
benches =
  bgroup
    "repeat-with-index"
    [ bench "list" $ nf list 0,
      bench "stream" $ nf stream 0,
      bench "vector" $ nf vector 0,
      bench "recursion" $ nf recursion 0
    ]
