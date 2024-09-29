module Tests.MaxFlow (tests) where

import AtCoder.MaxFlow qualified as MF
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hspec

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

  -- TODO: test min cut
  return ()

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

  return ()

-- TODO: flow twice

flowMax :: TestTree
flowMax = testCase "flowMax" $ do
  g <- MF.new 3
  (@?= 0) =<< MF.addEdge g 0 1 (maxBound :: Int)
  (@?= 1) =<< MF.addEdge g 1 0 maxBound
  (@?= 2) =<< MF.addEdge g 0 2 maxBound

  (@?= maxBound) =<< MF.flow g 0 2

  (@?= (0, 1, maxBound, 0)) =<< MF.getEdge g 0
  (@?= (1, 0, maxBound, 0)) =<< MF.getEdge g 1
  (@?= (0, 2, maxBound, maxBound)) =<< MF.getEdge g 2

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
  [ simple,
    notSimple,
    flowMax,
    unsafePerformIO invalidFlow
  ]
