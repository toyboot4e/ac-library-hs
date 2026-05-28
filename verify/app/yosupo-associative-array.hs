import AtCoder.Extra.HashMap qualified as HM
import Data.Maybe (fromMaybe)
import Data.Vector.Unboxed qualified as VU
import Util

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/associative_array
main :: IO ()
main = do
  q <- int
  qs <- VU.replicateM q $ do
    withLine $ do
      intP >>= \case
        0 -> (0 :: Int,,) <$> intP <*> intP
        1 -> (1,,-1) <$> intP
        _ -> error "unreachable"

  hm <- HM.new q
  res <- (`VU.mapMaybeM` qs) $ \case
    (0, !k, !v) -> do
      HM.insert hm k v
      pure Nothing
    (1, !k, !_) -> do
      Just . fromMaybe 0 <$> HM.lookup hm k
    _ -> error "unreachable"

  printBSB $ unlinesBSB res
