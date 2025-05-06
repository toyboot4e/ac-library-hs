import AtCoder.String qualified as AS
import Data.ByteString.Char8 qualified as BS
import Data.Vector.Unboxed qualified as VU

countUniqueSubstringsBS :: BS.ByteString -> Int
countUniqueSubstringsBS bs = (n * (n + 1)) `div` 2 - VU.sum lcp
  where
    n = BS.length bs
    sa = AS.suffixArrayBS bs
    lcp = AS.lcpArrayBS bs sa

-- verification-helper: PROBLEM https://atcoder.jp/contests/practice2/tasks/practice2_i
main :: IO ()
main = do
  s <- BS.getLine
  print $ countUniqueSubstringsBS s
