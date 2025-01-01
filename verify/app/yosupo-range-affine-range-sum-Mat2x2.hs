import AtCoder.Extra.Monoid.Mat2x2 qualified as Mat2x2
import AtCoder.Extra.Monoid.V2 qualified as V2
import AtCoder.LazySegTree qualified as LST
import AtCoder.ModInt qualified as M
import Data.Vector.Unboxed qualified as VU
import Util

type Mint = M.ModInt998244353

modInt :: Int -> Mint
modInt = M.new

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/range_affine_range_sum
main :: IO ()
main = do
  (!_, !q) <- ints2
  xs <- VU.map (V2.new . modInt) <$> ints
  qs <- VU.replicateM q $ withLine $ do
    intP >>= \case
      0 -> (0 :: Int,,,,) <$> intP <*> intP <*> intP <*> intP
      1 -> (1,,,-1,-1) <$> intP <*> intP
      _ -> error "unreachable"

  seg <- LST.build @_ @(Mat2x2.Mat2x2 Mint) @(V2.V2 Mint) xs
  res <- (`VU.mapMaybeM` qs) $ \case
    (0, !l, !r, !a, !b) -> do
      LST.applyIn seg l r $ Mat2x2.new (modInt a) (modInt b)
      pure Nothing
    (1, !l, !r, !_, !_) -> do
      x <- V2.unV2 <$> LST.prod seg l r
      pure $ Just $ M.val x
    _ -> error "unreachable"

  printBSB $ unlinesBSB res
