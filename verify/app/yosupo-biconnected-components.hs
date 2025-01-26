import AtCoder.Extra.Graph qualified as Gr
import Data.ByteString.Builder qualified as BSB
import Data.Foldable (for_)
import Data.Vector.Unboxed qualified as VU
import Util

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/biconnected_components
main :: IO ()
main = do
  (!n, !m) <- ints2
  es <- VU.replicateM m ints2
  let gr = Gr.build' n $ Gr.swapDupe' es
  let bct = Gr.blockCut n (gr `Gr.adj`)
  print $ Gr.nCsr bct - n
  for_ [n .. Gr.nCsr bct - 1] $ \k -> do
    let vs = bct `Gr.adj` k
    let h = BSB.intDec (VU.length vs) <> wsBSB
    printBSB . (h <>) $ unwordsBSB vs
