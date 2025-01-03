import AtCoder.Extra.Graph qualified as Gr
import AtCoder.Extra.Tree.Hld qualified as Hld
import AtCoder.Extra.Tree.TreeMonoid qualified as TM
import Data.Semigroup (Sum (..))
import Data.Vector.Unboxed qualified as VU
import Util

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/vertex_add_path_sum
main :: IO ()
main = do
  (!n, !q) <- ints2
  xs <- ints
  es <- VU.replicateM (n - 1) ints2
  qs <- VU.replicateM q ints3

  let !gr = Gr.build' n $ Gr.swapDupe' es
  let !hld = Hld.new gr
  tm <- TM.fromVerts hld TM.Commute $ VU.map Sum xs

  res <- (`VU.mapMaybeM` qs) $ \case
    (0, !p, !x) -> do
      TM.modify tm (<> Sum x) p
      pure Nothing
    (1, !u, !v) -> do
      Just . getSum <$> TM.prod tm u v
    _ -> error "unreachable"

  printBSB $ unlinesBSB res
