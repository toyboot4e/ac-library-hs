import AtCoder.Extra.Tree.Lct qualified as Lct
import Data.Semigroup (Sum (..))
import Data.Vector.Unboxed qualified as VU
import Util

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/dynamic_tree_vertex_add_subtree_sum
main :: IO ()
main = do
  (!n, !q) <- ints2
  xs <- ints
  uvs <- VU.replicateM (n - 1) ints2
  qs <-
    VU.replicateM q $
      withLine $
        intP >>= \case
          0 -> (0 :: Int,,,,) <$> intP <*> intP <*> intP <*> intP
          1 -> (1,,,-1,-1) <$> intP <*> intP
          2 -> (2,,,-1,-1) <$> intP <*> intP
          _ -> error "unreachable"

  lct <- Lct.buildInv negate (VU.map Sum xs) uvs
  res <- (`VU.mapMaybeM` qs) $ \case
    (0, !u, !v, !w, !x) -> do
      -- delete edge (u, v)
      Lct.cut lct u v
      -- add edge (w, x)
      Lct.link lct w x
      pure Nothing
    (1, !v, !x, !_, !_) -> do
      -- add to subtree vertices
      Lct.modify lct (<> Sum x) v
      pure Nothing
    (2, !v, !p, !_, !_) -> do
      -- output sub of subtree under @v@ whose parent is @p@.
      Sum res <- Lct.prodSubtree lct v p
      pure $ Just res
    _ -> error "unreachable"

  printBSB $ unlinesBSB res
