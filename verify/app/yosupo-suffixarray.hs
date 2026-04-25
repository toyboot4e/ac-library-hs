import AtCoder.String qualified as S
import Data.ByteString.Char8 qualified as BS
import Util

-- competitive-verifier: PROBLEM https://judge.yosupo.jp/problem/suffixarray
main :: IO ()
main = do
  s <- BS.getLine
  printBSB . unwordsBSB $ S.suffixArrayBS s
