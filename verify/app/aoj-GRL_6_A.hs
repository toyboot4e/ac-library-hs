import AtCoder.MaxFlow qualified as MF
import Data.Vector.Unboxed qualified as VU
import Util

-- verification-helper: PROBLEM https://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=GRL_6_A
-- max flow
main :: IO ()
main = do
  (!v, !e) <- ints2
  edges <- VU.replicateM e ints3
  graph <- MF.new v
  VU.forM_ edges $ \(!u, !v, !c) -> do
    MF.addEdge_ graph u v c
  res <- MF.flow graph 0 (v - 1) maxBound
  print res
