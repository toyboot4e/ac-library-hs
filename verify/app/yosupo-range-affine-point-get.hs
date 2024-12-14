import AtCoder.Extra.Monoid (Affine1 (..))
import AtCoder.LazySegTree qualified as LST
import AtCoder.ModInt qualified as M
import Data.Semigroup (Sum (..))
import Data.Vector.Unboxed qualified as VU
import Util

type Mint = M.ModInt998244353

modInt :: Int -> Mint
modInt = M.new

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/range_affine_point_get
main :: IO ()
main = do
  (!_, !q) <- ints2
  xs <- VU.map (Sum . modInt) <$> ints
  qs <- VU.replicateM q $ withLine $ do
    intP >>= \case
      0 -> (0 :: Int,,,,) <$> intP <*> intP <*> intP <*> intP
      1 -> (1,,-1,-1,-1) <$> intP
      _ -> error "unreachable"

  seg <- LST.build xs
  res <- (`VU.mapMaybeM` qs) $ \case
    (0, !l, !r, !a, !b) -> do
      LST.applyIn seg l r $ Affine1 (modInt a, modInt b)
      pure Nothing
    (1, !i, !_, !_, !_) -> do
      Sum x <- LST.read seg i
      pure $ Just $ M.val x
    _ -> error "unreachable"

  printBSB $ unlinesBSB res
