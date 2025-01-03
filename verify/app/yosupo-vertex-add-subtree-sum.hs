import AtCoder.Extra.Graph qualified as Gr
import AtCoder.Extra.Tree.Hld qualified as Hld
import AtCoder.Extra.Tree.TreeMonoid qualified as TM
import Data.Semigroup (Sum (..))
import Data.Vector.Unboxed qualified as VU
import Util

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/vertex_add_subtree_sum
main :: IO ()
main = do
  (!n, !q) <- ints2
  xs <- ints
  ps <- ints
  qs <-
    VU.replicateM q
      $ withLine
      $ intP >>= \case
        0 -> (0 :: Int,,) <$> intP <*> intP
        1 -> (1 :: Int,,-1) <$> intP
        _ -> error "unreachable"

  let es = VU.imap (\to from -> (from, to + 1)) ps
  let gr = Gr.build' n $ Gr.swapDupe' es
  let hld = Hld.new gr
  tm <- TM.fromVerts hld TM.Commute $ VU.map Sum xs

  res <- (`VU.mapMaybeM` qs) $ \case
    (0, !i, !x) -> do
      TM.modify tm (<> Sum x) i
      pure Nothing
    (1, !r, !_) -> do
      Just <$> TM.prodSubtree tm r
    _ -> error "unreachable"
  printBSB . unlinesBSB $ VU.map getSum res
