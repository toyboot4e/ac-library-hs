module Main (main) where

import Test.Tasty (testGroup)
import Test.Tasty.Ingredients.Rerun
import Tests.Convolution qualified
import Tests.Dsu qualified
import Tests.Extra.Bisect qualified
import Tests.Extra.HashMap qualified
import Tests.Extra.IntMap qualified
import Tests.Extra.IntSet qualified
import Tests.Extra.Math qualified
import Tests.Extra.Monoid qualified
import Tests.FenwickTree qualified
import Tests.Internal.Bit qualified
import Tests.Internal.Buffer qualified
import Tests.Internal.GrowVec qualified
import Tests.Internal.Math qualified
import Tests.Internal.McfCsr qualified
import Tests.Internal.MinHeap qualified
import Tests.Internal.Queue qualified
import Tests.LazySegTree qualified
import Tests.LazySegTreeStress qualified
import Tests.Math qualified
import Tests.MaxFlow qualified
import Tests.MinCostFlow qualified
import Tests.ModInt qualified
import Tests.Scc qualified
import Tests.SegTree qualified
import Tests.String qualified
import Tests.TwoSat qualified

main :: IO ()
main =
  defaultMainWithRerun
    . testGroup "toplevel"
    $ [ testGroup "Convolution" Tests.Convolution.tests,
        testGroup "Dsu" Tests.Dsu.tests,
        testGroup
          "Extra"
          [ testGroup "Bisect" Tests.Extra.Bisect.tests,
            testGroup "HashMap" Tests.Extra.HashMap.tests,
            testGroup "IntMap" Tests.Extra.IntMap.tests,
            testGroup "IntSet" Tests.Extra.IntSet.tests,
            testGroup "Math" Tests.Extra.Math.tests,
            testGroup "Monoid" Tests.Extra.Monoid.tests
          ],
        testGroup "FenwickTree" Tests.FenwickTree.tests,
        testGroup
          "Internal"
          [ testGroup "Bit" Tests.Internal.Bit.tests,
            testGroup "Buffer" Tests.Internal.Buffer.tests,
            testGroup "GrowVec" Tests.Internal.GrowVec.tests,
            testGroup "Math" Tests.Internal.Math.tests,
            testGroup "MinHeap" Tests.Internal.MinHeap.tests,
            testGroup "McfCsr" Tests.Internal.McfCsr.tests,
            testGroup "Queue" Tests.Internal.Queue.tests
          ],
        testGroup "LazySegTree" Tests.LazySegTree.tests,
        testGroup "LazySegTreeStress" Tests.LazySegTreeStress.tests,
        testGroup "Math" Tests.Math.tests,
        testGroup "MaxFlow" Tests.MaxFlow.tests,
        testGroup "MinCostFlow" Tests.MinCostFlow.tests,
        testGroup "ModInt" Tests.ModInt.tests,
        testGroup "Scc" Tests.Scc.tests,
        testGroup "SegTree" Tests.SegTree.tests,
        testGroup "String" Tests.String.tests,
        testGroup "TwoSat" Tests.TwoSat.tests
      ]
