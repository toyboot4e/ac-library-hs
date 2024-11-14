{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

import AtCoder.SegTree qualified as ST
import Data.Semigroup
import Data.Vector.Unboxed qualified as VU
import Util

-- verification-helper: PROBLEM https://atcoder.jp/contests/practice2/tasks/practice2_j
main :: IO ()
main = do
  (!n, !q) <- ints2
  xs <- VU.map Max <$> ints
  qs <- VU.replicateM q ints3

  -- seg <- ST.build xs
  seg <- ST.new n
  VU.iforM_ xs $ \i x -> do
    ST.write seg i x

  res <- (`VU.mapMaybeM` qs) $ \case
    (1, pred -> !x, !v) -> do
      ST.write seg x $ Max v
      pure Nothing
    (2, pred -> !l, !r) -> do
      Just . getMax <$> ST.prod seg l r
    (3, pred -> !p, !target) -> do
      Just . (+ 1) <$> ST.maxRight seg p (< Max target)
    _ -> error "unreachable"
  printBSB $ unlinesBSB res
