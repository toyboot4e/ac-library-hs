import AtCoder.Extra.WaveletMatrix.SegTree qualified as WM
import Data.Semigroup (Sum (..))
import Data.Vector.Unboxed qualified as VU
import Util

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/point_add_rectangle_sum
-- #wavelet-matrix-2d
main :: IO ()
main = do
  (!n, !q) <- ints2
  xyws <- VU.replicateM n ((\(!x, !y, !w) -> (x, y, Sum w)) <$> ints3)
  qs <- VU.replicateM q $ do
    withLine $
      intP >>= \case
        0 -> (0 :: Int,,,,-1 :: Int) <$> intP <*> intP <*> intP
        1 -> (1 :: Int,,,,) <$> intP <*> intP <*> intP <*> intP
        _ -> error "unreachable"

  wm <-
    WM.build negate
      . (xyws VU.++)
      $ VU.mapMaybe
        ( \case
            (0, !x, !y, !_, !_) -> Just (x, y, mempty)
            _ -> Nothing
        )
        qs

  res <-
    VU.mapMaybeM
      ( \case
          (0, !x, !y, !w, !_) -> do
            WM.modify wm (<> Sum w) (x, y)
            return Nothing
          (1, !x1, !y1, !x2, !y2) -> do
            Just . getSum <$> WM.prod wm x1 x2 y1 y2
          _ -> error "unreachable"
      )
      qs

  printBSB $ unlinesBSB res
