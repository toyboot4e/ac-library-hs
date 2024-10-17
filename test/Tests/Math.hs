-- | Fenwick tree tests.
module Tests.Math (tests) where

import AtCoder.Math
import Data.Foldable
import Test.Tasty
import Test.Tasty.HUnit

floorSumNaive :: Int -> Int -> Int -> Int -> Int
floorSumNaive n m a b = sum [(a * i + b) `div` m | i <- [0 .. n - 1]]

floorSumTest :: TestTree
floorSumTest = testCase "floorSum" $ do
  for_ [0 .. 20 - 1] $ \n -> do
    for_ [1 .. 20 - 1] $ \m -> do
      for_ [-20 .. 19] $ \a -> do
        for_ [-20 .. 19] $ \b -> do
          floorSumNaive n m a b @?= floorSum n m a b

tests :: [TestTree]
tests = [floorSumTest]
