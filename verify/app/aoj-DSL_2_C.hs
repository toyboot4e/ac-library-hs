{-# LANGUAGE ViewPatterns #-}

import AtCoder.Extra.KdTree qualified as Kt
import AtCoder.Extra.Monoid (RangeAdd (..))
import Data.Semigroup (Sum (..))
import Data.Vector.Algorithms.Intro qualified as VAI
import Data.Vector.Unboxed qualified as VU
import Util

-- verification-helper: PROBLEM https://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=DSL_2_C
main :: IO ()
main = do
  n <- int
  (!xs, !ys) <- VU.unzip <$> VU.replicateM n ints2
  q <- int
  qs <- VU.replicateM q ints4

  let kt = Kt.build xs ys
  VU.forM_ qs $ \(!xl, succ -> xr, !yl, succ -> yr) -> do
    let vs = VU.modify VAI.sort $ Kt.findPointsIn kt xl xr yl yr n
    if VU.null vs
      then putStrLn ""
      else printBSB $ unlinesBSB vs <> endlBSB
