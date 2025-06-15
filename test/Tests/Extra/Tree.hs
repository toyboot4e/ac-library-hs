module Tests.Extra.Tree where

import AtCoder.Extra.Graph qualified as Gr
import AtCoder.Extra.Tree qualified as Tree
import Data.Bit (Bit (..))
import Data.Ord (Down (..), comparing)
import Data.Semigroup (Sum (..))
import Data.Vector.Unboxed qualified as VU
import Test.Tasty
import Test.Tasty.HUnit

unit_one :: TestTree
unit_one = testCase "one" $ do
  let n = 1
  let !t = Gr.build @Int n VU.empty
  Tree.diameter n (Gr.adjW t) (-1) @?= ((0, 0), 0)
  Tree.diameterPath 1 (Gr.adjW t) (-1) @?= (VU.singleton 0, 0)
  Tree.mst n (VU.singleton (0, 0, 100)) @?= (0, VU.singleton (Bit False), t)
  Tree.mstBy (comparing Down) 1 (VU.singleton (0, 0, 100)) @?= (0, VU.singleton (Bit False), t)
  Tree.fold (Gr.adjW t) (const (Sum (10 :: Int))) (\(Sum a) (!_, !dw) -> Sum (a + dw)) (<>) 0 @?= 10
  Tree.scan n (Gr.adjW t) (const (Sum (10 :: Int))) (\(Sum a) (!_, !dw) -> Sum (a + dw)) (<>) 0 @?= VU.singleton 10
  Tree.foldReroot n (Gr.adjW t) (const (Sum (10 :: Int))) (\(Sum a) (!_, !dw) -> Sum (a + dw)) (<>) @?= VU.singleton 10

tests :: [TestTree]
tests =
  [ unit_one
  ]
