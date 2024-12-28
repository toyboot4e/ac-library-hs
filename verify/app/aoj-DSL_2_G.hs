import AtCoder.Extra.Monoid (RangeAdd (..))
import AtCoder.LazySegTree qualified as LST
import Data.Semigroup (Sum (..))
import Data.Vector.Unboxed qualified as VU
import Util

-- verification-helper: PROBLEM https://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=DSL_2_G&lang=ja
main :: IO ()
main = do
  (!n, !q) <- ints2
  qs <- VU.replicateM q $ withLine $ do
    intP >>= \case
      0 -> (0 :: Int,,,) <$> intP <*> intP <*> intP
      1 -> (1 :: Int,,,-1) <$> intP <*> intP
      _ -> error "unreachable"

  seg <- LST.build $ VU.replicate n (Sum (0 :: Int))
  res <- (`VU.mapMaybeM` qs) $ \case
    (0, !l, !r, !x) -> do
      LST.applyIn seg (l - 1) r $ RangeAdd (Sum x)
      pure Nothing
    (1, !l, !r, !_) -> do
      Sum x <- LST.prod seg (l - 1) r
      pure $ Just x
    _ -> error "unreachable"

  printBSB $ unlinesBSB res
