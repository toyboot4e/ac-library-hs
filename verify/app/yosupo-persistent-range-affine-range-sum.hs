import AtCoder.Extra.DynLazySegTree.Persistent qualified as Ldst
import AtCoder.Extra.Monoid.Affine1 (Affine1 (..))
import AtCoder.Extra.Monoid.Affine1 qualified as Affine1
import AtCoder.ModInt qualified as M
import Data.Semigroup (Sum (..))
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import Util

type Mint = M.ModInt998244353

modInt :: Int -> Mint
modInt = M.new

capacityFor :: Int -> Int -> Int
capacityFor n q = 5 * q * max 2 (ceiling (logBase 2 (fromIntegral n) :: Double))

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/persistent_range_affine_range_sum
main :: IO ()
main = do
  (!n, !q) <- ints2
  xs <- VU.map (Sum . modInt) <$> ints
  qs <- VU.replicateM q $ withLine $ do
    intP >>= \case
      0 -> (0 :: Int,,,,,) <$> intP <*> intP <*> intP <*> intP <*> intP
      1 -> (1,,,,,-1) <$> intP <*> intP <*> intP <*> intP
      2 -> (2,,,,-1,-1) <$> intP <*> intP <*> intP
      _ -> error "unreachable"

  -- let cap = 10 ^ 7 :: Int
  let cap = capacityFor n q
  seg <- Ldst.new @_ @(Affine1 Mint) @(Sum Mint) cap 0 n
  root0 <- Ldst.newSeq seg xs

  roots <- VUM.unsafeNew $ q + 1
  VGM.write roots 0 root0

  res <- (`VU.imapMaybeM` qs) $ \i query -> case query of
    (0, !k, !l, !r, !a, !b) -> do
      rootK <- VGM.read roots (k + 1)
      root <- Ldst.applyIn seg rootK l r $ Affine1.new (modInt a) (modInt b)
      VGM.write roots (i + 1) root
      pure Nothing
    (1, !k, !s, !l, !r, !_) -> do
      rootK <- VGM.read roots (k + 1)
      rootS <- VGM.read roots (s + 1)
      root <- Ldst.copyInterval seg rootK rootS l r
      VGM.write roots (i + 1) root
      pure Nothing
    (2, !k, !l, !r, !_, !_) -> do
      rootK <- VGM.read roots (k + 1)
      VGM.write roots (i + 1) rootK
      Sum x <- Ldst.prod seg rootK l r
      pure $ Just $ M.val x
    _ -> error "unreachable"

  printBSB $ unlinesBSB res
