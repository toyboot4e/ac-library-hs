import AtCoder.Extra.Graph qualified as Gr
import Data.ByteString.Builder qualified as BSB
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import Util

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/biconnected_components
main :: IO ()
main = do
  (!n, !m) <- ints2
  es <- VU.replicateM m ints2
  let gr = Gr.build' n $ Gr.swapDupe' es
  let bct = Gr.blockCutComponents n (gr `Gr.adj`)
  print $ V.length bct
  V.forM_ bct $ \vs -> do
    let h = BSB.intDec (VU.length vs) <> wsBSB
    printBSB . (h <>) $ unwordsBSB vs
