{-# LANGUAGE RecordWildCards #-}

module Tests.Scc (tests) where

import AtCoder.Internal.Csr qualified as ACICSR
import AtCoder.Scc qualified as Scc
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec
import Test.QuickCheck.Monadic qualified as QC
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck as QC

unit_empty :: TestTree
unit_empty = testCase "empty" $ do
  graph0 <- Scc.new 0
  (@?= V.empty) =<< Scc.scc graph0
  return ()

-- assign

unit_simple :: TestTree
unit_simple = testCase "simple" $ do
  graph <- Scc.new 2
  Scc.addEdge graph (0, 1)
  Scc.addEdge graph (1, 0)
  scc <- Scc.scc graph
  V.length scc @?= 1

unit_selfLoop :: TestTree
unit_selfLoop = testCase "selfLoop" $ do
  graph <- Scc.new 2
  Scc.addEdge graph (0, 0)
  Scc.addEdge graph (0, 0)
  Scc.addEdge graph (1, 1)
  scc <- Scc.scc graph
  V.length scc @?= 2

spec_invalid :: IO TestTree
spec_invalid = testSpec "invalid" $ do
  g <- runIO $ Scc.new 2
  it "throws error" $ do
    Scc.addEdge g (0, 10) `shouldThrow` anyException

-- Tests after this line are not in the original ac-library

unit_order :: TestTree
unit_order = testCase "order" $ do
  graph <- Scc.new 4
  Scc.addEdge graph (0, 1)
  Scc.addEdge graph (1, 2)
  Scc.addEdge graph (2, 3)
  Scc.addEdge graph (3, 0)
  scc <- Scc.scc graph
  scc @?= V.singleton (VU.fromList [0, 1, 2, 3])

unit_sccOrder :: TestTree
unit_sccOrder = testCase "sccOrder" $ do
  graph <- Scc.new 4
  Scc.addEdge graph (0, 1)
  Scc.addEdge graph (1, 0)
  Scc.addEdge graph (2, 1)
  Scc.addEdge graph (3, 2)
  scc <- Scc.scc graph
  -- The SCCs are topologically sorted
  scc @?= V.fromList [VU.singleton 3, VU.singleton 2, VU.fromList [0, 1]]

unit_multipleSelfLoops :: TestTree
unit_multipleSelfLoops = testCase "multipleSelfLoops" $ do
  graph <- Scc.new 2
  Scc.addEdge graph (1, 0)
  Scc.addEdge graph (1, 1)
  Scc.addEdge graph (0, 0)
  Scc.addEdge graph (0, 0)
  Scc.addEdge graph (0, 0)
  Scc.addEdge graph (0, 0)
  scc <- Scc.scc graph
  scc @?= V.fromList [VU.singleton 1, VU.singleton 0]

tests :: [TestTree]
tests =
  [ unit_empty,
    unit_simple,
    unit_selfLoop,
    unsafePerformIO spec_invalid,
    unit_order,
    unit_sccOrder,
    unit_multipleSelfLoops
  ]
