import AtCoder.Extra.Seq qualified as Seq
import Control.Monad
import Control.Monad.ST (runST)
import Data.ByteString qualified as BS
import Data.Semigroup (Sum (..))
import Data.Vector.Unboxed qualified as VU
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
        seq <- Seq.new (n + q)
        root <- Seq.newSeq @_ @() seq $ VU.map Sum xs
        (`VU.mapMaybeM` qs) $ \case
          (0, !l, !r) -> do
            Seq.reverse seq root l r
            pure Nothing
          (1, !l, !r) -> do
            Sum x <- Seq.prod seq root l r
            pure $ Just x

  printBSB $ unlinesBSB res
