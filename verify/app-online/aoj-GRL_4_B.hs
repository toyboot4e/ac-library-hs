import AtCoder.Extra.Graph qualified as Gr
import AtCoder.Internal.Csr qualified as Csr
import Data.Vector.Unboxed qualified as VU
import Util

-- verification-helper: PROBLEM https://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=GRL_4_B
main :: IO ()
main = do
  (!n, !m) <- ints2
  es <- VU.replicateM m ((\(!u, !v) -> (u, v, ())) <$> ints2)
  let gr = Csr.build n es
  printBSB $ unlinesBSB $ Gr.topSort n (gr `Csr.adj`)
