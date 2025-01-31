import AtCoder.Extra.Monoid.Affine1 qualified as Affine1
import AtCoder.Extra.Seq.Raw qualified as Seq
import AtCoder.ModInt qualified as M
import Control.Monad.ST (runST)
import Data.Semigroup (Sum (..))
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import Data.Vector.Generic.Mutable qualified as VGM
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
        seq <- Seq.newST @(Affine1.Affine1 Mint) @(Sum Mint) (n + q)
        rt0 <- Seq.newSeqST seq $ VU.map (Sum . M.new @998244353) xs
        root <- VUM.replicate 1 rt0

        (`VU.mapMaybeM` qs) $ \case
          (0, !i, !x, !_, !_) -> do
            -- insert
            rt <- VGM.read root 0
            rt' <- Seq.insertST seq rt i (Sum (M.new x))
            VGM.write root 0 rt'
            pure Nothing
          (1, !i, !_, !_, !_) -> do
            -- delete
            rt <- VGM.read root 0
            -- rt' <- Seq.deleteST_ seq rt i
            rt' <- Seq.detachST seq rt i
            VGM.write root 0 rt'
            pure Nothing
          (2, !l, !r, !_, !_) -> do
            -- reverse
            rt <- VGM.read root 0
            rt' <- Seq.reverseST seq rt l r
            VGM.write root 0 rt'
            pure Nothing
          (3, !l, !r, !b, !c) -> do
            -- apply affine transformation
            rt <- VGM.read root 0
            rt' <- Seq.applyInST seq rt l r (Affine1.new (M.new b) (M.new c))
            VGM.write root 0 rt'
            pure Nothing
          (4, !l, !r, !_, !_) -> do
            -- prod
            rt <- VGM.read root 0
            (Sum !x, !rt') <- Seq.prodST seq rt l r
            VGM.write root 0 rt'
            pure . Just $ M.val x
          _ -> error "unreachable"

  printBSB $ unlinesBSB res
