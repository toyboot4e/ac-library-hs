import AtCoder.Extra.Monoid.Affine1 (Affine1 (..))
import AtCoder.Extra.Monoid.Affine1 qualified as Affine1
import AtCoder.Extra.Seq qualified as Seq
import AtCoder.ModInt qualified as M
import Control.Monad.ST (runST)
import Data.Semigroup (Sum (..))
import Data.Vector.Unboxed qualified as VU
import Util

type Mint = M.ModInt 998244353

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/dynamic_sequence_range_affine_range_sum
main :: IO ()
main = do
  (!n, !q) <- ints2
  xs <- ints
  qs <- VU.replicateM q $ do
    withLine $
      intP >>= \case
        0 -> (0 :: Int,,,-1,-1) <$> intP <*> intP
        1 -> (1 :: Int,,-1,-1,-1) <$> intP
        2 -> (2 :: Int,,,-1,-1) <$> intP <*> intP
        3 -> (3 :: Int,,,,) <$> intP <*> intP <*> intP <*> intP
        4 -> (4 :: Int,,,-1,-1) <$> intP <*> intP
        _ -> error "unreachable"

  let res = runST $ do
        seq <- Seq.new @_ @(Affine1 Mint) @(Sum Mint) (n + q)
        root <- Seq.newSeq seq $ VU.map (Sum . M.new @998244353) xs

        (`VU.mapMaybeM` qs) $ \q -> case q of
          (0, !i, !x, !_, !_) -> do
            -- insert
            Seq.insert seq root i (Sum (M.new x))
            pure Nothing
          (1, !i, !_, !_, !_) -> do
            -- delete
            Seq.delete_ seq root i
            pure Nothing
          (2, !l, !r, !_, !_) -> do
            -- reverse
            Seq.reverse seq root l r
            pure Nothing
          (3, !l, !r, !b, !c) -> do
            -- apply affine transformation
            Seq.applyIn seq root l r (Affine1.new (M.new b) (M.new c))
            pure Nothing
          (4, !l, !r, !_, !_) -> do
            -- prod
            Sum !x <- Seq.prod seq root l r
            pure . Just $ M.val x
          _ -> error "unreachable"

  printBSB $ unlinesBSB res
