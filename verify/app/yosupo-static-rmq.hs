import AtCoder.Extra.SparseTable qualified as Tbl
import Data.Semigroup (Min (..))
import Data.Vector.Unboxed qualified as VU
import Util

-- competitive-verifier: PROBLEM https://judge.yosupo.jp/problem/staticrmq
main :: IO ()
main = do
  (!n, !q) <- ints2
  xs <- ints
  lrs <- VU.replicateM q ints2

  let tbl = Tbl.new $ VU.map Min xs
  printBSB . unlinesBSB $ VU.map (getMin . uncurry (Tbl.prod tbl)) lrs
