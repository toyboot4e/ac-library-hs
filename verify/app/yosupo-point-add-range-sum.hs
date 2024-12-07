import AtCoder.SegTree qualified as ST
import Data.Semigroup (Sum (..))
import Data.Vector.Unboxed qualified as VU
import Util

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/point_add_range_sum
main :: IO ()
main = do
  (!_, !q) <- ints2
  xs <- VU.map Sum <$> ints
  qs <- VU.replicateM q ints3

  seg <- ST.build xs
  res <- (`VU.mapMaybeM` qs) $ \case
    (0, !p, !x) -> do
      ST.modify seg (+ Sum x) p
      return Nothing
    (1, !l, !r) -> do
      Sum x <- ST.prod seg l r
      return $ Just x
    _ -> error "unreachable"

  printBSB $ unlinesBSB res
