import AtCoder.MinCostFlow qualified as Mcf
import Control.Monad (unless)
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Char8 qualified as BS
import Data.Foldable
import Data.List qualified as L
import Data.Ord (Down (..))
import Data.Vector qualified as V
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import Util

-- verification-helper: PROBLEM https://atcoder.jp/contests/past202109-open/tasks/past202109_k
main :: IO ()
main = do
  (!h, !w) <- ints2
  !mat <- V.replicateM h BS.getLine
  !ys <- VU.replicateM h ints2
  !xs <- VU.replicateM w ints2

  let !s0 = VU.sum (VU.map snd ys) + VU.sum (VU.map snd xs)
  let !dys = VU.map (uncurry (-)) ys
  let !dxs = VU.map (uncurry (-)) xs

  -- Maximum weight matching in a bipartite graph.
  -- It's just a maximum cost flow problem, where we have weight and capacity as distinct values.
  let !targetFlow = h -- min h w
  let [!y0, !x0, !src, !sink, !nVerts] = L.scanl' (+) (0 :: Int) [h, w, 1, 1]

  -- edge: (v1, v2, capacity, cost)
  let es1 = VU.generate h $ \iy -> (src, y0 + iy, 1, 0)
  let es2 = VU.generate w $ \ix -> (x0 + ix, sink, 1, 0)
  let es3 = VU.concat . V.toList $ (`V.imap` mat) $ \y bs ->
        (`VU.mapMaybe` VU.generate w id) $ \x ->
          let !dw = dys VU.! y + dxs VU.! x
              !c = BS.index bs x
           in -- REMARK: remove negative edges
              if c == '0' || dw <= 0
                then Nothing
                else Just (y0 + y, x0 + x, 1, dw)

  -- because maximum flow is not always the best, we make up a bipass edge:
  let bipass = VU.singleton (src, sink, targetFlow, 0)
  let es' = bipass VU.++ es1 VU.++ es2 VU.++ es3

  g <- Mcf.new nVerts
  VU.forM_ es' $ \(!u, !v, !cap, !cost) -> do
    Mcf.addEdge_ g u v cap cost
  -- (!_, Down !delta) <- Mcf.flow g src sink targetFlow
  -- print $ s0 + delta
  res <- Mcf.slope g src sink targetFlow
  print res
