import AtCoder.Extra.Graph qualified as Gr
import Data.Vector.Unboxed qualified as VU
import Util

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/shortest_path
main :: IO ()
main = do
  (!n, !m, !src, !sink) <- ints4
  es <- VU.replicateM m ints3

  let !gr = Gr.build n es
  let (!dists, !parents) = Gr.trackingDijkstra n (Gr.adjW gr) m (-1) $ VU.singleton (src, 0)

  case dists VU.! sink of
    (-1) -> putStrLn "-1"
    d -> do
      let !pathVerts = Gr.constructPathFromRoot parents sink
      printBSB . unwordsBSB $ VU.fromList [d, VU.length pathVerts - 1]
      printBSB . unlinesWithBSB show2 $ VU.zip pathVerts (VU.tail pathVerts)
