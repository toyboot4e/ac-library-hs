import AtCoder.Extra.Seq.Raw qualified as Seq
import Data.ByteString qualified as BS
import Control.Monad
import Control.Monad.ST (runST)
import Data.Semigroup (Sum (..))
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import System.Exit (exitSuccess)
import Util

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/range_reverse_range_sum
main :: IO ()
main = do
  (!n, !q) <- ints2

  -- TODO: handle correctly
  when (q == 0) $ do
    exitSuccess

  -- TODO: handle correctly
  when (n == 0) $ do
    _ <- BS.getLine
    qs <- VU.replicateM q ints3
    let nq = VU.length $ VU.filter (\(!t, !_, !_) -> t == 1) qs
    printBSB $ unlinesBSB $ VU.replicate nq (0 :: Int)
    exitSuccess

  xs <- ints
  qs <- VU.replicateM q ints3

  let res = runST $ do
        seq <- Seq.newST (n + q)
        root0 <- Seq.newSeqST @() seq $ VU.map Sum xs

        root <- VUM.replicate 1 root0
        (`VU.mapMaybeM` qs) $ \case
          (0, !l, !r) -> do
            rt <- VUM.read root 0
            rt' <- Seq.reverseST seq rt l r
            VUM.write root 0 rt'
            pure Nothing
          (1, !l, !r) -> do
            rt <- VUM.read root 0
            (Sum !x, !rt') <- Seq.prodST seq rt l r
            VUM.write root 0 rt'
            pure $ Just x

  printBSB $ unlinesBSB res
