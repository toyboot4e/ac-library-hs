import AtCoder.Extra.Graph qualified as Gr
import Data.ByteString.Builder qualified as BSB
import Data.Vector.Unboxed qualified as VU
import Util

-- verification-helper: PROBLEM https://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=GRL_1_B
main :: IO ()
main = do
  (!n, !m, !src) <- ints3
  uvws <- VU.replicateM m ints3

  let !gr = Gr.build n uvws
  let !inf = maxBound `div` 2
  case Gr.bellmanFord n (Gr.adjW gr) (-inf) (VU.singleton (src, 0)) of
    Nothing -> do
      putStrLn "NEGATIVE CYCLE"
    Just dist -> do
      printBSB $
        unlinesWithBSB
          ( \d ->
              if d == -inf
                then BSB.string7 "INF"
                else BSB.intDec d
          )
          dist
