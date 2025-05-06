import AtCoder.Dsu qualified as Dsu
import Data.Vector.Unboxed qualified as VU
import Util

-- verification-helper: PROBLEM https://atcoder.jp/contests/practice2/tasks/practice2_a
main :: IO ()
main = do
  (!n, !q) <- ints2
  tuvs <- VU.replicateM q ints3

  dsu <- Dsu.new n
  res <- (`VU.mapMaybeM` tuvs) $ \case
    (0, !u, !v) -> do
      Dsu.merge_ dsu u v
      pure Nothing
    (1, !u, !v) -> do
      b <- Dsu.same dsu u v
      pure . Just $ if b then 1 else 0
    _ -> error "unreachable"

  printBSB $ unlinesBSB res
