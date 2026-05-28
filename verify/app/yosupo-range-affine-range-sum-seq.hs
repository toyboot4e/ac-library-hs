import AtCoder.Extra.Monoid.Affine1 qualified as Affine1
import AtCoder.Extra.Seq qualified as Seq
import AtCoder.ModInt qualified as M
import Data.Semigroup (Sum (..))
import Data.Vector.Unboxed qualified as VU
import Util

type Mint = M.ModInt998244353

modInt :: Int -> Mint
modInt = M.new

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/range_affine_range_sum
main :: IO ()
main = do
  (!n, !q) <- ints2
  xs <- VU.map (Sum . modInt) <$> ints
  qs <- VU.replicateM q $ withLine $ do
    intP >>= \case
      0 -> (0 :: Int,,,,) <$> intP <*> intP <*> intP <*> intP
      1 -> (1,,,-1,-1) <$> intP <*> intP
      _ -> error "unreachable"

  seq <- Seq.new n
  root <- Seq.newSeq seq xs
  res <- (`VU.mapMaybeM` qs) $ \case
    (0, !l, !r, !a, !b) -> do
      Seq.applyIn seq root l r $ Affine1.new (modInt a) (modInt b)
      pure Nothing
    (1, !l, !r, !_, !_) -> do
      Sum x <- Seq.prod seq root l r
      pure $ Just $ M.val x
    _ -> error "unreachable"

  printBSB $ unlinesBSB res
