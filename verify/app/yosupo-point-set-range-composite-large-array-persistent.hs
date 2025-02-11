import AtCoder.Extra.DynSegTree.Persistent qualified as Dst
import AtCoder.Extra.Monoid (Affine1 (..))
import AtCoder.Extra.Monoid.Affine1 qualified as Affine1
import AtCoder.ModInt qualified as M
import Data.Monoid (Dual (..))
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import Util

type Mint = M.ModInt998244353

modInt :: Int -> Mint
modInt = M.new

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/point_set_range_composite_large_array
main :: IO ()
main = do
  (!n, !q) <- ints2
  qs <- VU.replicateM q ints4

  let cap = Dst.recommededCapacity n q
  seg <- Dst.new @_ @(Dual (Affine1 Mint)) cap 0 n
  root0 <- Dst.newRoot seg
  handle <- VUM.replicate 1 root0

  res <- (`VU.mapMaybeM` qs) $ \case
    (0, !p, !c, !d) -> do
      root <- VUM.read handle 0
      VUM.write handle 0 =<< (Dst.write seg root p . Dual $ Affine1 (modInt c, modInt d))
      pure Nothing
    (1, !l, !r, !x) -> do
      root <- VUM.read handle 0
      Dual f <- Dst.prod seg root l r
      pure . Just . M.val $ Affine1.act f (modInt x)
    _ -> error "unreachable"

  printBSB $ unlinesBSB res
