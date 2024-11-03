module Main (main) where

import Test.Tasty (testGroup)
import Test.Tasty.Ingredients.Rerun
import Tests.Dsu qualified
import Tests.FenwickTree qualified
import Tests.Internal.Buffer qualified
import Tests.Internal.GrowVec qualified
import Tests.Internal.McfCsr qualified
import Tests.Internal.MinHeap qualified
import Tests.Internal.Queue qualified
import Tests.Math qualified
import Tests.MaxFlow qualified
import Tests.MinCostFlow qualified
import Tests.Scc qualified
import Tests.TwoSat qualified

main :: IO ()
main =
  defaultMainWithRerun
    . testGroup "toplevel"
    $ [ testGroup "Tests.Dsu" Tests.Dsu.tests,
        testGroup "Tests.FenwickTree" Tests.FenwickTree.tests,
        testGroup "Tests.Internal.Buffer" Tests.Internal.Buffer.tests,
        testGroup "Tests.Internal.GrowVec" Tests.Internal.GrowVec.tests,
        testGroup "Tests.Internal.MinHeap" Tests.Internal.MinHeap.tests,
        testGroup "Tests.Internal.McfCsr" Tests.Internal.McfCsr.tests,
        testGroup "Tests.Internal.Queue" Tests.Internal.Queue.tests,
        testGroup "Tests.Math" Tests.Math.tests,
        testGroup "Tests.MaxFlow" Tests.MaxFlow.tests,
        testGroup "Tests.MinCostFlow" Tests.MinCostFlow.tests,
        testGroup "Tests.Scc" Tests.Scc.tests,
        testGroup "Tests.TwoSat" Tests.TwoSat.tests
      ]
