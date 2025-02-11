import AtCoder.Extra.DynLazySegTree.Persistent qualified as Ldst
import AtCoder.Extra.Monoid (Affine1 (..))
import AtCoder.ModInt qualified as M
import Data.Semigroup (Sum (..))
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import Util

type Mint = M.ModInt998244353

modInt :: Int -> Mint
modInt = M.new

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/range_affine_range_sum_large_array
main :: IO ()
main = do
  (!n, !q) <- ints2
  qs <- VU.replicateM q $ withLine $ do
    intP >>= \case
      0 -> (0 :: Int,,,,) <$> intP <*> intP <*> intP <*> intP
      1 -> (1,,,-1,-1) <$> intP <*> intP
      _ -> error "unreachable"

  let cap = Ldst.recommendedCapacity n q
  seg <- Ldst.new @_ @(Affine1 Mint) @(Sum Mint) cap 0 n
  root0 <- Ldst.newRoot seg
  rootHandle <- VUM.replicate 1 root0

  res <- (`VU.mapMaybeM` qs) $ \case
    (0, !l, !r, !a, !b) -> do
      root <- VGM.read rootHandle 0
      root' <- Ldst.applyIn seg root l r $ Affine1 (modInt a, modInt b)
      VGM.write rootHandle 0 root'
      pure Nothing
    (1, !l, !r, !_, !_) -> do
      root <- VGM.read rootHandle 0
      Sum x <- Ldst.prod seg root l r
      pure $ Just $ M.val x
    _ -> error "unreachable"

  printBSB $ unlinesBSB res
