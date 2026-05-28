import AtCoder.Extra.Tree.Lct qualified as Lct
import Data.Semigroup (Sum (..))
import Data.Vector.Unboxed qualified as VU
import Util

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/dynamic_tree_vertex_add_path_sum
main :: IO ()
main = do
  (!n, !q) <- ints2
  xs <- VU.map Sum <$> ints
  -- uvs <- VU.replicateM (n - 1) $ withLine ((,) <$> mintP <*> mintP)
  uvs <- VU.replicateM (n - 1) ints2
  qs <-
    VU.replicateM q $
      withLine $
        intP >>= \case
          0 -> (0 :: Int,,,,) <$> intP <*> intP <*> intP <*> intP
          1 -> (1,,,-1,-1) <$> intP <*> intP
          2 -> (2,,,-1,-1) <$> intP <*> intP
          _ -> error "unreachable"

  lct <- Lct.build xs uvs
  res <- (`VU.mapMaybeM` qs) $ \case
    (0, !u, !v, !w, !x) -> do
      -- delete edge (u, v)
      Lct.cut lct u v
      -- add edge (w, x)
      Lct.link lct w x
      pure Nothing
    (1, !p, !dx, !_, !_) -> do
      -- ad dx
      Lct.modify lct (Sum dx <>) p
      pure Nothing
    (2, !u, !v, !x, !_) -> do
      -- Be sure to composite from @v@ to @u@ so that @f(u)@ is applied first:
      Sum res <- Lct.prodPath lct v u
      pure $ Just res
    _ -> error "unreachable"

  printBSB $ unlinesBSB res
