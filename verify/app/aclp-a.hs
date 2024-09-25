{-# LANGUAGE LambdaCase #-}

import AtCoder.DSU qualified as DSU
import Data.Vector.Unboxed qualified as VU
import Util

-- verification-helper: PROBLEM https://atcoder.jp/contests/practice2/tasks/practice2_a
main :: IO ()
main = do
  (!n, !q) <- ints2
  tuvs <- VU.replicateM q ints3

  dsu <- DSU.new n
  res <- (`VU.mapMaybeM` tuvs) $ \case
    (0, !u, !v) -> do
      DSU.merge dsu u v
      return Nothing
    (1, !u, !v) -> do
      b <- DSU.same dsu u v
      return . Just $ if b then 1 else 0
    _ -> error "unreachable"

  printBSB $ unlinesBSB res
