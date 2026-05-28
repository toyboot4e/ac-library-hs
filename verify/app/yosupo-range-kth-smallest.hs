import AtCoder.Extra.WaveletMatrix qualified as WM
import Data.Maybe (fromJust)
import Data.Vector.Unboxed qualified as VU
import Util

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/range_kth_smallest
main :: IO ()
main = do
  (!n, !q) <- ints2
  xs <- ints
  lrks <- VU.replicateM q ints3
  let !wm = WM.build xs
  let !res = VU.map (\(!l, !r, !k) -> fromJust $ WM.kthSmallestIn wm l r k) lrks
  printBSB $ unlinesBSB res
