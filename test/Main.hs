module Main (main) where

import Test.Tasty (testGroup)
import Test.Tasty.Ingredients.Rerun
import Tests.Convolution qualified
import Tests.Dsu qualified
import Tests.Extra.AhoCorasick qualified
import Tests.Extra.Bisect qualified
import Tests.Extra.DsuMonoid qualified
import Tests.Extra.DynLazySegTree qualified
import Tests.Extra.DynLazySegTree.Persistent qualified
import Tests.Extra.DynSegTree qualified
import Tests.Extra.DynSegTree.Persistent qualified
import Tests.Extra.DynSparseSegTree qualified
import Tests.Extra.DynSparseSegTree.Persistent qualified
import Tests.Extra.Graph qualified
import Tests.Extra.HashMap qualified
import Tests.Extra.Hld qualified
import Tests.Extra.IntMap qualified
import Tests.Extra.IntSet qualified
import Tests.Extra.IntervalMap qualified
import Tests.Extra.Ix0 qualified
import Tests.Extra.KdTree qualified
import Tests.Extra.LazyKdTree qualified
import Tests.Extra.Math qualified
import Tests.Extra.Math.Montgomery64 qualified
import Tests.Extra.ModInt64 qualified
import Tests.Extra.Monoid qualified
import Tests.Extra.MultiSet qualified
import Tests.Extra.Pdsu qualified
import Tests.Extra.SegTree2d qualified
import Tests.Extra.SegTree2d.Dense qualified
import Tests.Extra.Semigroup.Matrix qualified
import Tests.Extra.Semigroup.Permutation qualified
import Tests.Extra.Seq qualified
import Tests.Extra.Seq.Map qualified
import Tests.Extra.Tree qualified
import Tests.Extra.Tree.Lct qualified
import Tests.Extra.Vector qualified
import Tests.Extra.Vector.Prim qualified
import Tests.Extra.WaveletMatrix qualified
import Tests.Extra.WaveletMatrix.BitVector qualified
import Tests.Extra.WaveletMatrix.Raw qualified
import Tests.Extra.WaveletMatrix2d qualified
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
          [ testGroup "AhoCorasick" Tests.Extra.AhoCorasick.tests,
            testGroup "Bisect" Tests.Extra.Bisect.tests,
            testGroup "DsuMonoid" Tests.Extra.DsuMonoid.tests,
            testGroup "DynLazySegTree" Tests.Extra.DynLazySegTree.tests,
            testGroup "DynLazySegTree.Persistent" Tests.Extra.DynLazySegTree.Persistent.tests,
            testGroup "DynSegTree" Tests.Extra.DynSegTree.tests,
            testGroup "DynSegTree.Persistent" Tests.Extra.DynSegTree.Persistent.tests,
            testGroup "DynSparseSegTree" Tests.Extra.DynSparseSegTree.tests,
            testGroup "DynSparseSegTree.Persistent" Tests.Extra.DynSparseSegTree.Persistent.tests,
            testGroup "Graph" Tests.Extra.Graph.tests,
            testGroup "HashMap" Tests.Extra.HashMap.tests,
            testGroup "Hld" Tests.Extra.Hld.tests,
            testGroup "IntervalMap" Tests.Extra.IntervalMap.tests,
            testGroup "Ix0" Tests.Extra.Ix0.tests,
            testGroup "IntMap" Tests.Extra.IntMap.tests,
            testGroup "IntSet" Tests.Extra.IntSet.tests,
            testGroup "KdTree" Tests.Extra.KdTree.tests,
            testGroup "LazyKdTree" Tests.Extra.LazyKdTree.tests,
            testGroup "Math" Tests.Extra.Math.tests,
            testGroup "Math.Montgomery64" Tests.Extra.Math.Montgomery64.tests,
            testGroup "ModInt64" Tests.Extra.ModInt64.tests,
            testGroup "Monoid" Tests.Extra.Monoid.tests,
            testGroup "MultiSet" Tests.Extra.MultiSet.tests,
            testGroup "Pdsu" Tests.Extra.Pdsu.tests,
            testGroup "SegTree2d" Tests.Extra.SegTree2d.tests,
            testGroup "SegTree2d.Dense" Tests.Extra.SegTree2d.Dense.tests,
            testGroup "Semigroup.Matrix" Tests.Extra.Semigroup.Matrix.tests,
            testGroup "Semigroup.Permutation" Tests.Extra.Semigroup.Permutation.tests,
            testGroup "Seq" Tests.Extra.Seq.tests,
            testGroup "Seq.Map" Tests.Extra.Seq.Map.tests,
            testGroup "Tree" Tests.Extra.Tree.tests,
            testGroup "Tree.Lct" Tests.Extra.Tree.Lct.tests,
            testGroup "Vector" Tests.Extra.Vector.tests,
            testGroup "Vector.Prim" Tests.Extra.Vector.Prim.tests,
            testGroup "WaveletMatrix" Tests.Extra.WaveletMatrix.tests,
            testGroup "WaveletMatrix.BitVector" Tests.Extra.WaveletMatrix.BitVector.tests,
            testGroup "WaveletMatrix.Raw" Tests.Extra.WaveletMatrix.Raw.tests,
            testGroup "WaveletMatrix2d" Tests.Extra.WaveletMatrix2d.tests
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
