{-# LANGUAGE LambdaCase #-}

import AtCoder.LazySegTree (segAct)
import AtCoder.LazySegTree.Monoid (Affine2d (..))
import AtCoder.ModInt qualified as M
import AtCoder.SegTree qualified as ST
import Data.Monoid (Dual (..))
import Data.Semigroup (Sum (..))
import Data.Vector.Unboxed qualified as VU
import Util

type Mint = M.ModInt998244353

modInt :: Int -> Mint
modInt = M.new

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/point_set_range_composite
main :: IO ()
main = do
  (!n, !q) <- ints2
  xs <- VU.map (\(!a, !b) -> Dual (Affine2d (modInt a, modInt b))) <$> VU.replicateM n ints2
  qs <- VU.replicateM q ints4

  seg <- ST.build xs
  res <- (`VU.mapMaybeM` qs) $ \case
    (0, !p, !c, !d) -> do
      ST.write seg p . Dual $ Affine2d (modInt c, modInt d)
      pure Nothing
    (1, !l, !r, !x) -> do
      Dual f <- ST.prod seg l r
      -- FIXME: relax the Monoid constraint
      pure . Just . M.val . getSum $ f `segAct` Sum (modInt x)
    _ -> error "unreachable"

  printBSB $ unlinesBSB res
