import AtCoder.Extra.Graph qualified as Gr
import AtCoder.Extra.Tree qualified as Tr
import Data.Vector.Unboxed qualified as VU
import Util

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/tree_diameter
main :: IO ()
main = do
  n <- int
  uvws <- VU.replicateM (n - 1) ints3
  let gr = Gr.build n $ Gr.swapDupe uvws
  let (!path, !dist) = Tr.diameterPath n (gr `Gr.adjW`) (-1)
  printBSB $ unwordsBSB $ VU.fromList [dist, VU.length path]
  printBSB $ unwordsBSB path
