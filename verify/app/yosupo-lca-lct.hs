import AtCoder.Extra.Tree.Lct qualified as Lct
import Data.Vector.Unboxed qualified as VU
import Util

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/lca
main :: IO ()
main = do
  (!n, !q) <- ints2
  ps <- ints
  qs <- VU.replicateM q ints2
  let !lct = Lct.build (VU.replicate n ()) qs
  printBSB . unlinesBSB =<< VU.mapM (\(!u, !v) -> Lct.lca lct u v) qs
