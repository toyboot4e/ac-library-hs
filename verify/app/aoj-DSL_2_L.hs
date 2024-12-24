import AtCoder.Extra.Monoid (RangeSet (..))
import AtCoder.LazySegTree qualified as LST
import Data.Bit (Bit(..))
import Data.Semigroup (Sum (..))
import Data.Vector.Unboxed qualified as VU
import Util

-- verification-helper: PROBLEM https://onlinejudge.u-aizu.ac.jp/problems/DSL_2_I
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
      LST.applyIn seg l (r + 1) $ RangeSet (Bit True, Sum x)
      pure Nothing
    (1, !l, !r, !_) -> do
      Sum x <- LST.prod seg l (r + 1)
      pure $ Just x
    _ -> error "unreachable"

  printBSB $ unlinesBSB res
