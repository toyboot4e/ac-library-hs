import AtCoder.MinCostFlow qualified as MCF
import Data.Vector.Unboxed qualified as VU
import Util

-- verification-helper: PROBLEM https://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=GRL_6_B
-- min cost flow
main :: IO ()
main = do
  (!v, !e, !f) <- ints3
  edges <- VU.replicateM e ints4
  graph <- MCF.new v
  VU.forM_ edges $ \(!a, !b, !c, !d) -> do
    MCF.addEdge_ graph a b c d
  (!flow, !res) <- MCF.flow graph 0 (v - 1) f
  if flow == f
    then print res
    else putStrLn "-1"
