{-# LANGUAGE ViewPatterns #-}

import AtCoder.Extra.Monoid.Mat2x2 (Mat2x2 (..), unMat2x2)
import AtCoder.Extra.Monoid.Mat2x2 qualified as Mat2x2
import AtCoder.Extra.Wdsu qualified as Wdsu
import AtCoder.ModInt qualified as M
import Data.Bool (bool)
import Data.ByteString.Builder qualified as BSB
import Data.Semigroup (Dual (..))
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import Util

type Mint = M.ModInt 998244353

modInt :: Int -> Mint
modInt = M.unsafeNew . fromIntegral

show4 :: (Mint, Mint, Mint, Mint) -> BSB.Builder
show4 (!a, !b, !c, !d) = unwordsBSB (VU.map M.val (VU.fromListN 4 [a, b, c, d]))

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/unionfind_with_potential_non_commutative_group
main :: IO ()
main = do
  (!n, !q) <- ints2
  qs <- VU.replicateM q $ withLine $ do
    intP >>= \case
      0 -> (0 :: Int,,,) <$> intP <*> intP <*> int4P
      1 -> (1 :: Int,,,(-1, -1, -1, -1)) <$> intP <*> intP
      _ -> error "unreachable"

  uf <- Wdsu.new @_ @(Dual (Mat2x2 Mint)) n (Mat2x2.inv <$>)
  res <- (`V.mapM` VU.convert qs) $ \case
    (0, !u, !v, (!x00, !x01, !x10, !x11)) -> do
      let !dx = Dual $ Mat2x2 (modInt x00, modInt x01, modInt x10, modInt x11)
      b <- Wdsu.canMerge uf u v dx
      Wdsu.merge_ uf u v dx
      pure . BSB.char8 $ bool '0' '1' b
    (1, !u, !v, (!_, !_, !_, !_)) -> do
      maybe (BSB.string7 "-1") (show4 . unMat2x2 . getDual) <$> Wdsu.diff uf u v
    _ -> error "unreachable"

  printBSB $ unlinesWithBSB id res
