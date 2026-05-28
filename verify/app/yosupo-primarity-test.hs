import AtCoder.Extra.Math (isPrime)
import Data.Vector.Unboxed qualified as VU
import Util

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/primality_test
main :: IO ()
main = do
  q <- int
  ns <- VU.replicateM q int
  printBSB . unwordsWithBSB ynBSB $ VU.map isPrime ns
