import AtCoder.Convolution qualified as C
import Data.Vector.Unboxed qualified as VU
import Util

-- verification-helper: PROBLEM https://atcoder.jp/contests/past202203-open/tasks/past202203_n
main :: IO ()
main = do
  n <- int
  let !m = 200000 :: Int
  xs <- ints
  let !ps = VU.accumulate (+) (VU.replicate m (0 :: Int)) $ VU.map ((, 1) . pred) xs
  let !qs = VU.accumulate (+) (VU.replicate m (0 :: Int)) $ VU.map ((, 1) . (m -)) xs
  let !res = C.convolution64 ps qs
  print $ VU.length $ VU.filter (/= 0) res
