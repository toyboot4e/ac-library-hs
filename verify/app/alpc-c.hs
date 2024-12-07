import AtCoder.Math (floorSum)
import Data.Vector.Unboxed qualified as VU
import Util

-- verification-helper: PROBLEM https://atcoder.jp/contests/practice2/tasks/practice2_c
main :: IO ()
main = do
  t <- int
  nmabs <- VU.replicateM t ints4

  let res = VU.map (\(!n, !m, !a, !b) -> floorSum n m a b) nmabs
  printBSB $ unlinesBSB res

