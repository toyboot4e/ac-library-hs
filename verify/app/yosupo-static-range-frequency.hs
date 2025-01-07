import AtCoder.Extra.WaveletMatrix qualified as WM
import Data.Vector.Unboxed qualified as VU
import Util

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/static_range_frequency
-- #wavelet-matrix
main :: IO ()
main = do
  (!n, !q) <- ints2
  if n == 0
    then do
      printBSB . unlinesBSB $ VU.replicate q 0
    else do
      xs <- ints
      lrxs <- VU.replicateM q ints3
      let !wm = WM.build xs
      let res = VU.map (\(!l, !r, !x) -> WM.rank wm l r x) lrxs
      printBSB $ unlinesBSB res
