import AtCoder.Extra.SegTree2d qualified as Seg
import Data.Semigroup (Sum (..))
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import Util

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/point_add_rectangle_sum
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

  seg <-
    Seg.build3 . (xyws VU.++) $
      VU.mapMaybe
        ( \case
            (0, !x, !y, !_, !_) -> Just (x, y, mempty)
            _ -> Nothing
        )
        qs

  nextI <- VUM.replicate 1 n
  res <-
    VU.mapMaybeM
      ( \case
          (0, !x, !y, !w, !_) -> do
            i <- VGM.read nextI 0
            VGM.write nextI 0 $ i + 1
            Seg.write seg i $ Sum w
            pure Nothing
          (1, !x1, !y1, !x2, !y2) -> do
            Just . getSum <$> Seg.prod seg x1 x2 y1 y2
          _ -> error "unreachable"
      )
      qs

  printBSB $ unlinesBSB res
