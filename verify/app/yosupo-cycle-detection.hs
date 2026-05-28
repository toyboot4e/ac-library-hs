import AtCoder.Extra.Graph qualified as Gr
import Data.ByteString.Builder qualified as BSB
import Data.Vector.Unboxed qualified as VU
import Util

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/cycle_detection
main :: IO ()
main = do
  (!n, !m) <- ints2
  es <- VU.generateM m $ \i -> do
    (!u, !v) <- ints2
    pure (u, v, i)
  let gr = Gr.build n es
  case Gr.findCycleDirected gr of
    Nothing -> putStrLn "-1"
    Just (!vs, !es) -> do
      printBSB . BSB.intDec $ VU.length vs
      -- restore original edge IDs
      printBSB $ unlinesBSB $ VU.backpermute (Gr.wCsr gr) es
