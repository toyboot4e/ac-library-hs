import AtCoder.Extra.Tree.Lct qualified as Lct
import Data.Vector.Unboxed qualified as VU
import Util

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/lca
main :: IO ()
main = do
  (!n, !q) <- ints2
  ps <- ints
  qs <- VU.replicateM q ints2
  lct <- Lct.build @_ @() (VU.replicate n ()) $ VU.imap ((,) . (+ 1)) ps
  printBSB . unlinesBSB =<< VU.mapM (\(!u, !v) -> Lct.lca lct u v) qs
