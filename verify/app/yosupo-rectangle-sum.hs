import AtCoder.Extra.WaveletMatrix.SegTree qualified as WM
import Data.Semigroup (Sum (..))
import Data.Vector.Unboxed qualified as VU
import Util

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/rectangle_sum
-- #wavelet-matrix-2d
main :: IO ()
main = do
  (!n, !q) <- ints2
  xyws <- VU.replicateM n ((\(!x, !y, !w) -> (x, y, Sum w)) <$> ints3)
  qs <- VU.replicateM q ints4

  wm <- WM.build negate xyws
  res <-
    VU.mapM
      ( \(!x1, y1, !x2, !y2) -> do
          getSum <$> WM.prod wm x1 x2 y1 y2
      )
      qs
  printBSB $ unlinesBSB res
