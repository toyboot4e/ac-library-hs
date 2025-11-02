import AtCoder.Extra.Monoid (RangeSet (..))
import AtCoder.LazySegTree qualified as LSeg
import Data.Bit (Bit (..))
import Data.Semigroup (Max (..))
import Data.Vector.Unboxed qualified as VU
import Util

-- verification-helper: PROBLEM https://atcoder.jp/contests/typical90/tasks/typical90_ac
main :: IO ()
main = do
  (!w, !n) <- ints2
  lrs <- VU.replicateM n ints2

  seg <- LSeg.build $ VU.replicate (w + 1) (Max (0 :: Int))
  res <- VU.forM lrs $ \(!l, !r) -> do
    Max !h <- LSeg.prod seg l (r + 1)
    LSeg.applyIn seg l (r + 1) $ RangeSet (Bit True, Max (h + 1))
    pure $ h + 1

  printBSB $ unlinesBSB res
