import AtCoder.Extra.Monoid.RangeAdd (RangeAdd (..))
import AtCoder.LazySegTree qualified as LSeg
import Data.Semigroup (Min (..))
import Data.Vector.Unboxed qualified as VU
import Util

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/range_add_range_min
main :: IO ()
main = do
  (!_, !q) <- ints2
  xs <- VU.map Min <$> ints
  qs <- VU.replicateM q $ withLine $ do
    intP >>= \case
      0 -> (0 :: Int,,,) <$> intP <*> intP <*> intP
      1 -> (1,,,-1) <$> intP <*> intP
      _ -> error "unreachable"

  seg <- LSeg.build xs
  res <- (`VU.mapMaybeM` qs) $ \case
    (0, !l, !r, !x) -> do
      LSeg.applyIn seg l r $ RangeAdd (Min x)
      pure Nothing
    (1, !l, !r, !_) -> do
      Min x <- LSeg.prod seg l r
      pure $ Just x
    _ -> error "unreachable"

  printBSB $ unlinesBSB res
