import AtCoder.Extra.Graph qualified as Gr
import AtCoder.Extra.Tree.Hld qualified as Hld
import Data.Vector.Unboxed qualified as VU
import Util

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/lca
main :: IO ()
main = do
  (!n, !q) <- ints2
  ps <- ints
  qs <- VU.replicateM q ints2
  let !gr = Gr.build' n . Gr.swapDupe' $ VU.imap ((,) . succ) ps
  let !hld = Hld.new gr
  printBSB . unlinesBSB $ VU.map (\(!u, !v) -> Hld.lca hld u v) qs
