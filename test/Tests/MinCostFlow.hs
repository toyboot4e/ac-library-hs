-- | Min cost flow tests.
module Tests.MinCostFlow (tests) where

import AtCoder.MinCostFlow qualified as MCF
import Data.Vector.Unboxed qualified as VU
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hspec

unit_zero :: TestTree
unit_zero = testCase "zero" $ do
  _g <- MCF.new @_ @Int @Int 0
  return ()

unit_simple :: TestTree
unit_simple = testCase "simple" $ do
  g <- MCF.new @_ @Int 4
  (@?= 0) =<< MCF.addEdge g 0 1 (1 :: Int) (1 :: Int)
  (@?= 1) =<< MCF.addEdge g 0 2 (1 :: Int) (1 :: Int)
  (@?= 2) =<< MCF.addEdge g 1 3 (1 :: Int) (1 :: Int)
  (@?= 3) =<< MCF.addEdge g 2 3 (1 :: Int) (1 :: Int)
  (@?= 4) =<< MCF.addEdge g 1 2 (1 :: Int) (1 :: Int)

  let expected = VU.fromListN 2 [(0, 0), (2, 4)]
  (@?= expected) =<< MCF.slope g 0 3 10

  ((0, 1, 1, 1, 1) @?=) =<< MCF.getEdge g 0
  ((0, 2, 1, 1, 1) @?=) =<< MCF.getEdge g 1
  ((1, 3, 1, 1, 1) @?=) =<< MCF.getEdge g 2
  ((2, 3, 1, 1, 1) @?=) =<< MCF.getEdge g 3
  ((1, 2, 1, 0, 1) @?=) =<< MCF.getEdge g 4

unit_usage :: TestTree
unit_usage = testCase "usage" $ do
  do
    g <- MCF.new @_ @Int 2
    (@?= 0) =<< MCF.addEdge g 0 1 (1 :: Int) (2 :: Int)
    (@?= (1, 2)) =<< MCF.flow g 0 1 maxBound
  do
    g <- MCF.new @_ @Int 2
    (@?= 0) =<< MCF.addEdge g 0 1 (1 :: Int) (2 :: Int)
    let expected = VU.fromList [(0, 0), (1, 2)]
    (@?= expected) =<< MCF.slope g 0 1 maxBound

-- assign is skipped

spec_outOfRange :: IO TestTree
spec_outOfRange = testSpec "outOfRange" $ do
  g <- runIO $ MCF.new @_ @Int @Int 10
  it "throws error" $ do
    MCF.slope g (-1) 3 maxBound `shouldThrow` anyException
  it "throws error" $ do
    MCF.slope g 3 3 maxBound `shouldThrow` anyException

unit_selfLoop :: TestTree
unit_selfLoop = testCase "selfLoop" $ do
  g <- MCF.new @_ @Int 3
  (@?= 0) =<< MCF.addEdge g 0 0 (100 :: Int) (123 :: Int)
  (@?= (0, 0, 100, 0, 123)) =<< MCF.getEdge g 0

unit_sameCostPaths :: TestTree
unit_sameCostPaths = testCase "sameCostPaths" $ do
  g <- MCF.new @_ @Int @Int 3
  (0 @?=) =<< MCF.addEdge g 0 1 1 1
  (1 @?=) =<< MCF.addEdge g 1 2 1 0
  (2 @?=) =<< MCF.addEdge g 0 2 2 1
  let expected = VU.fromList [(0, 0), (3, 3)]
  (@?= expected) =<< MCF.slope g 0 2 maxBound

spec_invalid :: IO TestTree
spec_invalid = testSpec "invalid" $ do
  g <- runIO $ MCF.new @_ @Int @Int 2
  it "throws error" $ do
    MCF.addEdge g 0 0 (-1) 0 `shouldThrow` anyException
  it "throws error" $ do
    MCF.addEdge g 0 0 0 (-1) `shouldThrow` anyException

-- TODO: test `edges` and `unsafeEdges`
-- TODO: stress test

tests :: [TestTree]
tests =
  [ unit_zero,
    unit_simple,
    unit_usage,
    unsafePerformIO spec_outOfRange,
    unit_selfLoop,
    unit_sameCostPaths,
    unsafePerformIO spec_invalid
  ]
