import AtCoder.Scc qualified as Scc
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import Util

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/scc
main :: IO ()
main = do
  (!n, !m) <- ints2
  es <- VU.replicateM m ints2
  sccGr <- Scc.new n
  VU.forM_ es (Scc.addEdge sccGr)
  scc <- Scc.scc sccGr
  print $ V.length scc
  V.forM_ scc $ \vs -> do
    printBSB $ unwordsBSB $ VU.cons (VU.length vs) vs
