import AtCoder.Extra.Graph qualified as Gr
import Data.Vector.Unboxed qualified as VU
import Util

-- verification-helper: PROBLEM https://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=GRL_1_C
main :: IO ()
main = do
  (!n, !m, !src) <- ints3
  uvws <- VU.replicateM m ints3

  let !gr = Gr.build n uvws
  printBSB $ unlinesBSB res
