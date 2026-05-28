import Data.Bool (bool)
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Char8 qualified as BS
import Data.IntSet qualified as IS
import Data.Maybe (fromMaybe)
import Data.Vector.Unboxed qualified as VU
import Util

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/predecessor_problem
main :: IO ()
main = do
  (!n, !q) <- ints2
  t <- BS.getLine
  qs <- VU.replicateM q ints2

  let set0 = IS.fromList . map fst . filter (\(!_, !c) -> c == '1') $ zip [0 :: Int ..] (BS.unpack t)
  VU.foldM'_
    ( \set q -> do
        case q of
          (0, !k) -> do
            pure $ IS.insert k set
          (1, !k) -> do
            pure $ IS.delete k set
          (2, !k) -> do
            printBSB . BSB.intDec . bool (0 :: Int) 1 $ IS.member k set
            pure set
          (3, !k) -> do
            printBSB . BSB.intDec . fromMaybe (-1) $ IS.lookupGE k set
            pure set
          (4, !k) -> do
            printBSB . BSB.intDec . fromMaybe (-1) $ IS.lookupLE k set
            pure set
          _ -> error "unreachable"
    )
    set0
    qs
