module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import Tests.DSU qualified
import Tests.FenwickTree qualified
import Tests.Math qualified
import Tests.MaxFlow qualified
import Tests.MinCostFLow qualified

main :: IO ()
main =
  defaultMain
    . testGroup "toplevel"
    $ [ testGroup "Tests.DSU" Tests.DSU.tests,
        testGroup "Tests.FenwickTree" Tests.FenwickTree.tests,
        testGroup "Tests.Math" Tests.DSU.tests,
        testGroup "Tests.MaxFlow" Tests.MaxFlow.tests,
        testGroup "Tests.MinCostFlow" Tests.MinCostFlow.tests
      ]
