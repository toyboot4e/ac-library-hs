import AtCoder.Extra.Graph qualified as Gr
import Data.Bit (Bit (..))
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import Util

-- verification-helper: https://atcoder.jp/contests/abc282/tasks/abc282_d
main :: IO ()
main = do
  (!n, !m) <- ints2
  !es <- VU.replicateM m $ do
    u <- int
    v <- int
    pure (u - 1, v - 1)

  let !gr = Gr.build' n $ Gr.swapDupe' es
  case Gr.bipartiteVertexColors n (gr `Gr.adj`) of
    Nothing -> do
      putStrLn "0"
    Just colors -> do
      let nMax = n * (n - 1) `div` 2
      let nIncompatible =
            V.sum
              . V.map
                ( \vs ->
                    let n1 = VU.length $ VU.filter (unBit . (colors VU.!)) vs
                        n2 = VU.length vs - n1
                     in n1 * (n1 - 1) `div` 2 + n2 * (n2 - 1) `div` 2
                )
              $ Gr.connectedComponents n (Gr.adj gr)
      print $ nMax - nIncompatible - m
