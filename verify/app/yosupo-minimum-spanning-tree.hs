import AtCoder.Extra.Tree qualified as Tree
import Data.Bit (unBit)
import Data.Vector.Unboxed qualified as VU
import Util

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/minimum_spanning_tree
main :: IO ()
main = do
  (!n, !m) <- ints2
  uvws <- VU.replicateM m ints3
  let (!w, !edgeUse, !_) = Tree.mst n uvws
  print w
  printBSB . unwordsBSB $ VU.findIndices unBit edgeUse
