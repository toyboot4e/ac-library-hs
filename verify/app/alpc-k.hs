{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

import AtCoder.LazySegTree qualified as LST
import AtCoder.LazySegTree.Monoid (Affine2d (..))
import AtCoder.ModInt qualified as ModInt
import Data.Semigroup
import Data.Vector.Unboxed qualified as VU
import Util

type Mint = ModInt.ModInt998244353

modInt :: Int -> Mint
modInt = ModInt.new

-- verification-helper: PROBLEM https://atcoder.jp/contests/practice2/tasks/practice2_k
main :: IO ()
main = do
  (!_, !q) <- ints2
  xs <- ints
  qs <- VU.replicateM q $ do
    withLine $
      intP >>= \case
        0 -> (0 :: Int,,,,) <$> intP <*> intP <*> intP <*> intP
        1 -> (1,,,-1,-1) <$> intP <*> intP
        _ -> error "unreachable"

  lst <- LST.build @_ @(Affine2d Mint) @(Sum Mint) $ VU.map (Sum . modInt) xs

  res <- (`VU.mapMaybeM` qs) $ \case
    (0, !l, !r, !a, !b) -> do
      LST.applyIn lst l r $ Affine2d (modInt a, modInt b)
      pure Nothing
    (1, !l, !r, -1, -1) -> do
      Sum x <- LST.prod lst l r
      pure . Just $ ModInt.val x
    _ -> error "unreachable"

  printBSB $ unlinesBSB res
