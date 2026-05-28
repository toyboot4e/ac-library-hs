import AtCoder.Extra.Math (primitiveRoot)
import Data.Vector.Unboxed qualified as VU
import Util

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/primitive_root
main :: IO ()
main = do
  q <- int
  ps <- VU.replicateM q int
  printBSB $ unlinesBSB $ VU.map primitiveRoot ps
