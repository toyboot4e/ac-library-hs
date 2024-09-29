{-# LANGUAGE LambdaCase #-}

import AtCoder.FenwickTree qualified as FT
import Data.Vector.Unboxed qualified as VU
import Util

-- verification-helper: PROBLEM https://atcoder.jp/contests/practice2/tasks/practice2_b
main :: IO ()
main = do
  (!n, !q) <- ints2
  xs <- ints
  qs <- VU.replicateM q ints3

  ft <- FT.new n
  VU.iforM_ xs $ \i x -> do
    FT.add ft i x

  res <- (`VU.mapMaybeM` qs) $ \case
    (0, !p, !x) -> do
      FT.add ft p x
      return Nothing
    (1, !l, !r) -> do
      x <- FT.sum ft l r
      return $ Just x
    _ -> error "unreachable"

  printBSB $ unlinesBSB res
