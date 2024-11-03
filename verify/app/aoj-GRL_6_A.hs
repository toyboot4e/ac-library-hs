import AtCoder.MaxFlow qualified as MF
import Control.Monad (unless, when)
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Char8 qualified as BS
import Data.Foldable
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
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
  res <- MF.flow graph 0 (v - 1)
  print res
