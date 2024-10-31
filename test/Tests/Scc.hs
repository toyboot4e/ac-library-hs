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

unit_selfLoop2 :: TestTree
unit_selfLoop2 = testCase "selfLoop2" $ do
  graph <- Scc.new 2
  Scc.addEdge graph (1, 0)
  Scc.addEdge graph (1, 1)
  Scc.addEdge graph (0, 0)
  scc <- Scc.scc graph
  scc @?= V.fromList [VU.singleton 1, VU.singleton 0]

unit_selfLoop3 :: TestTree
unit_selfLoop3 = testCase "selfLoop3" $ do
  graph <- Scc.new 2
  Scc.addEdge graph (1, 0)
  Scc.addEdge graph (1, 1)
  Scc.addEdge graph (0, 0)
  Scc.addEdge graph (0, 0)
  Scc.addEdge graph (0, 0)
  Scc.addEdge graph (0, 0)
  scc <- Scc.scc graph
  scc @?= V.fromList [VU.singleton 1, VU.singleton 0]

edgeGen :: Int -> Int -> QC.Gen [(Int, Int)]
-- edgeGen n m = fmap (filter (\(!u, !v) -> u /= v)) $ QC.vectorOf m $ do
edgeGen n m = QC.vectorOf m $ do
  from <- QC.chooseInt (0, n - 1)
  to <- QC.chooseInt (0, n - 1)
  return (from, to)

-- | Tests \(v_{i} \in adj(v_{j<i})\) for SCC components \(v_1, .., v_n\).
prop_order :: TestTree
prop_order = QC.testProperty "order" $ QC.monadicIO $ do
  n <- QC.pick $ QC.chooseInt (1, 2) -- FIXME: 16
  m <- QC.pick $ QC.chooseInt (0, 128)
  edges <- QC.pick $ VU.fromList <$> edgeGen n m
  let graph = ACICSR.build n edges
  scc <- QC.run $ do
    sccGraph <- Scc.new n
    VU.forM_ edges $ Scc.addEdge sccGraph
    Scc.scc sccGraph
  V.forM_ scc $ \vs -> do
    (`QC.assertWith` show (vs, ACICSR.adj graph 0))
      . VU.all
        ( \(!i, !v) ->
            i == 0 || VU.any (VU.elem v . ACICSR.adj graph) (VU.take i vs)
        )
      $ VU.indexed vs

tests :: [TestTree]
tests =
  [ unit_empty,
    unit_simple,
    unit_selfLoop,
    unsafePerformIO spec_invalid,
    unit_selfLoop2,
    unit_selfLoop3,
    unit_order,
    prop_order
  ]
