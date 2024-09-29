-- | Max flow tests.
module Tests.MaxFlow (tests) where

import AtCoder.MaxFlow qualified as MF
import Data.Vector.Unboxed qualified as VU
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hspec

zero :: TestTree
zero = testCase "zero" $ do
  g <- MF.new @Int 0
  h <- MF.new @Double 0
  return ()

-- Assign is skipped

simple :: TestTree
simple = testCase "simple" $ do
  g <- MF.new 4
  (@?= 0) =<< MF.addEdge g 0 1 (1 :: Int)
  (@?= 1) =<< MF.addEdge g 0 2 1
  (@?= 2) =<< MF.addEdge g 1 3 1
  (@?= 3) =<< MF.addEdge g 2 3 1
  (@?= 4) =<< MF.addEdge g 1 2 1

  -- TODO: combine the builder and the graph
  (@?= 2) =<< MF.flow g 0 3

  (@?= (0, 1, 1, 1)) =<< MF.getEdge g 0
  (@?= (0, 2, 1, 1)) =<< MF.getEdge g 1
  (@?= (1, 3, 1, 1)) =<< MF.getEdge g 2
  (@?= (2, 3, 1, 1)) =<< MF.getEdge g 3
  (@?= (1, 2, 1, 0)) =<< MF.getEdge g 4

  (@?= VU.fromList [True, False, False, False]) =<< MF.minCut g 0

notSimple :: TestTree
notSimple = testCase "notSimple" $ do
  g <- MF.new 2
  (@?= 0) =<< MF.addEdge g 0 1 (1 :: Int)
  (@?= 1) =<< MF.addEdge g 0 1 2
  (@?= 2) =<< MF.addEdge g 0 1 3
  (@?= 3) =<< MF.addEdge g 0 1 4
  (@?= 4) =<< MF.addEdge g 0 1 5
  (@?= 5) =<< MF.addEdge g 0 0 6
  (@?= 6) =<< MF.addEdge g 1 1 7

  (@?= 15) =<< MF.flow g 0 1

  (@?= (0, 1, 1, 1)) =<< MF.getEdge g 0
  (@?= (0, 1, 2, 2)) =<< MF.getEdge g 1
  (@?= (0, 1, 3, 3)) =<< MF.getEdge g 2
  (@?= (0, 1, 4, 4)) =<< MF.getEdge g 3
  (@?= (0, 1, 5, 5)) =<< MF.getEdge g 4

  (@?= VU.fromList [True, False]) =<< MF.minCut g 0

minCut :: TestTree
minCut = testCase "minCut" $ do
  g <- MF.new 3
  (@?= 0) =<< MF.addEdge g 0 1 (2 :: Int)
  (@?= 1) =<< MF.addEdge g 1 2 1
  (@?= 1) =<< MF.flow g 0 2

  (@?= (0, 1, 2, 1)) =<< MF.getEdge g 0
  (@?= (1, 2, 1, 1)) =<< MF.getEdge g 1

  (@?= VU.fromList [True, True, False]) =<< MF.minCut g 0

twice :: TestTree
twice = testCase "twice" $ do
  g <- MF.new 3
  (@?= 0) =<< MF.addEdge g 0 1 (1 :: Int)
  (@?= 1) =<< MF.addEdge g 0 2 1
  (@?= 2) =<< MF.addEdge g 1 2 1

  (@?= 2) =<< MF.flow g 0 2

  (@?= (0, 1, 1, 1)) =<< MF.getEdge g 0
  (@?= (0, 2, 1, 1)) =<< MF.getEdge g 1
  (@?= (1, 2, 1, 1)) =<< MF.getEdge g 2

  MF.changeEdge g 0 100 10
  (@?= (0, 1, 100, 10)) =<< MF.getEdge g 0

  (@?= 0) =<< MF.flow g 0 2
  (@?= 90) =<< MF.flow g 0 1

  (@?= (0, 1, 100, 100)) =<< MF.getEdge g 0
  (@?= (0, 2, 1, 1)) =<< MF.getEdge g 1
  (@?= (1, 2, 1, 1)) =<< MF.getEdge g 2

  (@?= 2) =<< MF.flow g 2 0

  (@?= (0, 1, 100, 99)) =<< MF.getEdge g 0
  (@?= (0, 2, 1, 0)) =<< MF.getEdge g 1
  (@?= (1, 2, 1, 0)) =<< MF.getEdge g 2

maxFlowBound :: TestTree
maxFlowBound = testCase "maxFlowBound" $ do
  g <- MF.new 3
  (@?= 0) =<< MF.addEdge g 0 1 (maxBound :: Int)
  (@?= 1) =<< MF.addEdge g 1 0 maxBound
  (@?= 2) =<< MF.addEdge g 0 2 maxBound

  (@?= maxBound) =<< MF.flow g 0 2

  (@?= (0, 1, maxBound, 0)) =<< MF.getEdge g 0
  (@?= (1, 0, maxBound, 0)) =<< MF.getEdge g 1
  (@?= (0, 2, maxBound, maxBound)) =<< MF.getEdge g 2

-- BoundUInt is skipped

selfLoop :: TestTree
selfLoop = testCase "selfLoop" $ do
  g <- MF.new 3
  (@?= 0) =<< MF.addEdge g 0 0 (100 :: Int)
  (@?= (0, 0, 100, 0)) =<< MF.getEdge g 0

invalidFlow :: IO TestTree
invalidFlow = testSpec "invalidFlow" $ do
  g <- runIO $ MF.new @Int 2
  it "throws error" $ do
    MF.flow g 0 0 `shouldThrow` anyException
  it "throws error" $ do
    MF.flow' g 0 0 0 `shouldThrow` anyException

-- TODO: stress test

tests :: [TestTree]
tests =
  [ zero,
    -- assign,
    simple,
    notSimple,
    minCut,
    twice,
    maxFlowBound,
    -- maxFlowBoundUInt
    selfLoop,
    unsafePerformIO invalidFlow
    -- stress
  ]
