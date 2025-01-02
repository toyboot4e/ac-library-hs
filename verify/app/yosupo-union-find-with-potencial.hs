{-# LANGUAGE ViewPatterns #-}

import AtCoder.Extra.Pdsu qualified as Pdsu
import AtCoder.ModInt qualified as M
import Data.Bool (bool)
import Data.Semigroup (Sum (..))
import Data.Vector.Unboxed qualified as VU
import Util

type Mint = M.ModInt 998244353

modInt :: Int -> Mint
modInt = M.unsafeNew . fromIntegral

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/unionfind_with_potential
main :: IO ()
main = do
  (!n, !q) <- ints2
  qs <-
    VU.replicateM q $
      withLine $
        intP >>= \case
          0 -> (0 :: Int,,,) <$> intP <*> intP <*> intP
          1 -> (1 :: Int,,,-1 :: Int) <$> intP <*> intP
          _ -> error "unreachable"
  dsu <- Pdsu.new @_ @(Sum Mint) n negate
  res <- (`VU.mapM` qs) $ \case
    (0, !u, !v, Sum . modInt -> !dx) -> do
      b <- Pdsu.canMerge dsu u v dx
      Pdsu.merge_ dsu u v dx
      pure $ bool 0 1 b
    (1, !u, !v, !_) -> do
      maybe (-1 :: Int) (M.val . getSum) <$> Pdsu.diff dsu u v
    _ -> error "unreachable"
  printBSB $ unlinesBSB res
