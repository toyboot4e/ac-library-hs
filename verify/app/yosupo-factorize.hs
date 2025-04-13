import AtCoder.Extra.Math qualified as Math
import Data.Vector.Unboxed qualified as VU
import Util

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/factorize
main :: IO ()
main = do
  q <- int
  qs <- VU.replicateM q int

  VU.forM_ qs $ \x -> do
    let pns = Math.primeFactors x
    let ps = VU.concatMap (\(!p, !np) -> VU.replicate np p) pns
    printBSB . unwordsBSB $ VU.cons (VU.length ps) ps

