module Tests.Extra.Hld where

import AtCoder.Extra.Graph qualified as Gr
import AtCoder.Extra.Tree.Hld qualified as Hld
import AtCoder.Extra.Tree.TreeMonoid qualified as Tm
import Data.Monoid (Sum (..))
import Data.Vector.Unboxed qualified as VU
import Test.Tasty
import Test.Tasty.HUnit

unit_one :: TestTree
unit_one = testCase "one" $ do
  let hld = Hld.new $ Gr.build @() 1 VU.empty
  (@?= 0) $ Hld.lca hld 0 0
  (@?= 0) $ Hld.ancestor hld 0 0
  (@?= Just 0) $ Hld.jump hld 0 0 0
  (@?= 0) $ Hld.lengthBetween hld 0 0
  (@?= [0]) $ Hld.path hld 0 0
  (@?= [(0, 0)]) $ Hld.pathSegmentsInclusive Hld.WeightsAreOnVertices hld 0 0
  (@?= []) $ Hld.pathSegmentsInclusive Hld.WeightsAreOnEdges hld 0 0
  (@?= (0, 0)) $ Hld.subtreeSegmentInclusive hld 0
  (@?= True) $ Hld.isInSubtree hld 0 0

  tm <- Tm.fromVerts hld Tm.Commute (VU.singleton (Sum (1 :: Int)))
  (@?= Sum 1) =<< Tm.prod tm 0 0
  (@?= Sum 1) =<< Tm.read tm 0
  Tm.write tm 0 $ Sum 2
  (@?= Sum 2) =<< Tm.exchange tm 0 (Sum 3)
  Tm.modify tm (+ (Sum 1)) 0
  (@?= Sum 4) =<< Tm.read tm 0
  (@?= Sum 4) =<< Tm.prod tm 0 0

tests :: [TestTree]
tests =
  [ unit_one
  ]
