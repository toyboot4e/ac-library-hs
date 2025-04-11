import AtCoder.Extra.Math (primes)
import Data.ByteString qualified as BS
import Data.Vector.Unboxed qualified as VU
import Util

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/enumerate_primes
main :: IO ()
main = do
  (!n, !a, !b) <- ints3
  let ps = primes n
  let len = VU.length ps
  -- print p_{ia + b}
  let is = VU.unfoldr f b
        where
          f i
            | i < len = Just (i, i + a)
            | otherwise = Nothing
  printBSB . show2 $ (len, VU.length is)
  printBSB . unwordsBSB $ VU.backpermute ps is
