import AtCoder.Extra.Monoid.Affine1 qualified as Affine1
import AtCoder.Extra.Tree.Lct qualified as Lct
import AtCoder.ModInt qualified as M
import Data.Vector.Unboxed qualified as VU
import Util

-- | Parses an `Int`.
{-# INLINE affineP #-}
affineP :: Parser (Affine1.Affine1 (M.ModInt 998244353))
affineP = do
  x <- mintP
  y <- mintP
  pure $ Affine1.new x y

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/dynamic_tree_vertex_set_path_composite
main :: IO ()
main = do
  (!n, !q) <- ints2
  affines <- VU.replicateM n (withLine affineP)
  uvs <- VU.replicateM (n - 1) ints2
  qs <-
    VU.replicateM q $
      withLine $
        intP >>= \case
          0 -> (0 :: Int,,,,) <$> intP <*> intP <*> intP <*> intP
          1 -> (1,,,,-1) <$> intP <*> intP <*> intP
          2 -> (2,,,,-1) <$> intP <*> intP <*> intP
          _ -> error "unreachable"

  lct <- Lct.build affines uvs
  res <- (`VU.mapMaybeM` qs) $ \case
    (0, !u, !v, !w, !x) -> do
      -- delete edge (u, v)
      Lct.cut lct u v
      -- add edge (w, x)
      Lct.link lct w x
      pure Nothing
    (1, !p, !c, !d, !_) -> do
      -- set f(x) := cx + d
      Lct.write lct p $ Affine1.new (M.new c) (M.new d)
      pure Nothing
    (2, !u, !v, !x, !_) -> do
      -- Be sure to composite from @v@ to @u@ so that @f(u)@ is applied first:
      res <- Lct.prodPath lct v u
      pure . Just . M.val $ Affine1.act res (M.new x)
    _ -> error "unreachable"

  printBSB $ unlinesBSB res
