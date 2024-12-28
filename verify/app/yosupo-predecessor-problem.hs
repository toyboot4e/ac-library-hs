import AtCoder.Extra.IntSet qualified as IS
import Control.Monad (when)
import Data.Bool (bool)
import Data.ByteString.Char8 qualified as BS
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import Data.Vector.Unboxed qualified as VU
import Util

-- TODO: compare speed with `containers`.

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/predecessor_problem
main :: IO ()
main = do
  (!n, !q) <- ints2
  t <- BS.getLine
  qs <- VU.replicateM q ints2

  set <- IS.new n
  for_ (zip [0 :: Int .. ] (BS.unpack t)) $ \(!i, !c) -> do
    when (c == '1') $ do
      IS.insert set i

  res <- (`VU.mapMaybeM` qs) $ \case
    (0, !k) -> do
      IS.insert set k
      pure Nothing
    (1, !k) -> do
      IS.delete_ set k
      pure Nothing
    (2, !k) -> do
      Just . bool (0 :: Int) 1 <$> IS.member set k
    (3, !k) -> do
      Just . fromMaybe (-1) <$> IS.lookupGE set k
    (4, !k) -> do
      Just . fromMaybe (-1) <$> IS.lookupLE set k
    _ -> error "unreachable"

  printBSB $ unlinesBSB res
