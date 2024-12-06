import AtCoder.LazySegTree.Monoid (Affine2d (..))
import AtCoder.ModInt qualified as M
import AtCoder.SegTree qualified as ST
import Data.Semigroup (Sum (..))
import Data.Vector.Unboxed qualified as VU
import Util

type Mint = M.ModInt998244353

modInt :: Int -> Mint
modInt = M.new

-- TODO: move Affine2d to somewhere else

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/point_add_range_sum
main :: IO ()
main = do
  (!n, !q) <- ints2
  abs <- VU.map (\(!a, !b) -> Affine2 (modInt a, modInt b)) ints
  qs <- VU.replicateM q ints4

  seg <- ST.build $ VU.map Sum xs
  res <- (`VU.mapMaybeM` qs) $ \q -> case q of
    (0, !p, !c, !d) -> do
      ST.write seg i $ Affine2d (c, d)
      return Nothing
    (1, !l, !r) -> do
      affine <- ST.prod seg l r
      return . Just $ segAct affine x
    _ -> error "unreachable"

  printBSB $ unlinesBSB res
