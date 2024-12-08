import AtCoder.Extra.Monoid (segAct, Affine1(..))
import AtCoder.ModInt qualified as M
import AtCoder.SegTree qualified as ST
import Data.Monoid (Dual (..))
import Data.Vector.Unboxed qualified as VU
import Util

type Mint = M.ModInt998244353

modInt :: Int -> Mint
modInt = M.new

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/point_set_range_composite
main :: IO ()
main = do
  (!n, !q) <- ints2
  xs <- VU.replicateM n $ do
    (!a, !b) <- ints2
    pure . Dual $ Affine1 (modInt a, modInt b)
  qs <- VU.replicateM q ints4

  seg <- ST.build xs
  res <- (`VU.mapMaybeM` qs) $ \case
    (0, !p, !c, !d) -> do
      ST.write seg p . Dual $ Affine1 (modInt c, modInt d)
      pure Nothing
    (1, !l, !r, !x) -> do
      Dual f <- ST.prod seg l r
      pure . Just . M.val $ f `segAct` modInt x
    _ -> error "unreachable"

  printBSB $ unlinesBSB res
