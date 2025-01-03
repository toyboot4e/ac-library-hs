import AtCoder.Extra.Monoid.Affine1 qualified as Affine1
import AtCoder.Extra.Graph qualified as Gr
import AtCoder.Extra.Tree.Hld qualified as Hld
import AtCoder.Extra.Tree.TreeMonoid qualified as TM
import AtCoder.ModInt qualified as M
import Data.Semigroup (Dual (..))
import Data.Vector.Unboxed qualified as VU
import Util

type Mint = M.ModInt 998244353

modInt :: Int -> Mint
modInt = M.new

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/vertex_set_path_composite
main :: IO ()
main = do
  (!n, !q) <- ints2
  abs_ <- VU.replicateM n ints2
  es <- VU.replicateM (n - 1) ints2
  qs <- VU.replicateM q ints4

  let gr = Gr.build' n $ Gr.swapDupe' es
  let hld = Hld.new gr
  tm <- TM.fromVerts hld TM.NonCommute $ VU.map (\(!a, !b) -> Dual (Affine1.new (modInt a) (modInt b))) abs_

  res <- (`VU.mapMaybeM` qs) $ \case
    (0, !p, !c, !d) -> do
      TM.write tm p . Dual $ Affine1.new (modInt c) (modInt d)
      pure Nothing
    (1, !u, !v, !x) -> do
      Dual !f <- TM.prod tm u v
      pure . Just $ Affine1.act f (modInt x)
    _ -> error "unreachable"

  printBSB $ unlinesBSB $ VU.map M.val res
