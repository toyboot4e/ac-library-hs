-- | Min cost flow tests.
module Tests.MinCostFlow (tests) where

import AtCoder.MinCostFlow qualified as MCF
import Data.Vector.Unboxed qualified as VU
import Test.Tasty
import Test.Tasty.HUnit

zero :: TestTree
zero = testCase "zero" $ do
  _g <- MCF.new @Int @Int 0
  return ()

simple :: TestTree
simple = testCase "simple" $ do
  g <- MCF.new @Int 4
  (@?= 0) =<< MCF.addEdge g 0 1 (1 :: Int) (1 :: Int)
  (@?= 1) =<< MCF.addEdge g 0 2 (1 :: Int) (1 :: Int)
  (@?= 2) =<< MCF.addEdge g 1 3 (1 :: Int) (1 :: Int)
  (@?= 3) =<< MCF.addEdge g 2 3 (1 :: Int) (1 :: Int)
  (@?= 4) =<< MCF.addEdge g 1 2 (1 :: Int) (1 :: Int)

  let expected = VU.fromListN 2 [(0, 0), (2, 4)]
  res <- MCF.slope g 0 3 10
  res @?= expected

  ((0, 1, 1, 1, 1) @?=) =<< MCF.getEdge g 0
  ((0, 2, 1, 1, 1) @?=) =<< MCF.getEdge g 1
  ((1, 3, 1, 1, 1) @?=) =<< MCF.getEdge g 2
  ((2, 3, 1, 1, 1) @?=) =<< MCF.getEdge g 3
  ((1, 2, 1, 0, 1) @?=) =<< MCF.getEdge g 4

  return ()

tests :: [TestTree]
tests = [zero, simple]

