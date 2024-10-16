-- | Min cost flow tests.
module Tests.MinCostFlow (tests) where

import AtCoder.MinCostFlow qualified as MCF
import Data.Vector.Unboxed qualified as VU
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hspec

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

usage :: TestTree
usage = testCase "simple" $ do
  do
    g <- MCF.new @Int 2
    (@?= 0) =<< MCF.addEdge g 0 1 (1 :: Int) (2 :: Int)
    (@?= (1, 2)) =<< MCF.flow g 0 1 maxBound
  do
    g <- MCF.new @Int 2
    (@?= 0) =<< MCF.addEdge g 0 1 (1 :: Int) (2 :: Int)
    let expected = VU.fromList [(0, 0), (1, 2)]
    (@?= expected) =<< MCF.slope g 0 1 maxBound

-- assign is skipped

outOfRange :: IO TestTree
outOfRange = testSpec "outOfRange" $ do
  g <- runIO $ MCF.new @Int @Int 10
  it "throws error" $ do
    MCF.slope g (-1) 3 maxBound `shouldThrow` anyException
  it "throws error" $ do
    MCF.slope g 3 3 maxBound `shouldThrow` anyException

selfLoop :: TestTree
selfLoop = testCase "selfLoop" $ do
  g <- MCF.new @Int 3
  (@?= 0) =<< MCF.addEdge g 0 0 (100 :: Int) (123 :: Int)
  (@?= (0, 0, 100, 0, 123)) =<< MCF.getEdge g 0

sameCostPaths :: TestTree
sameCostPaths = testCase "sameCostPaths" $ do
  g <- MCF.new @Int @Int 3
  (0 @?=) =<< MCF.addEdge g 0 1 1 1
  (1 @?=) =<< MCF.addEdge g 1 2 1 0
  (2 @?=) =<< MCF.addEdge g 0 2 2 1
  let expected = VU.fromList [(0, 0), (3, 3)]
  (@?= expected) =<< MCF.slope g 0 2 maxBound

invalid :: IO TestTree
invalid = testSpec "invalid" $ do
  g <- runIO $ MCF.new @Int @Int 2
  it "throws error" $ do
    MCF.addEdge g 0 0 (-1) 0 `shouldThrow` anyException
  it "throws error" $ do
    MCF.addEdge g 0 0 0 (-1) `shouldThrow` anyException

-- TODO: streee test

tests :: [TestTree]
tests =
  [ zero,
    simple,
    usage,
    unsafePerformIO outOfRange,
    selfLoop,
    sameCostPaths,
    unsafePerformIO invalid
  ]
