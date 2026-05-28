import AtCoder.Convolution qualified as C
import AtCoder.ModInt qualified as Mint
import Data.ByteString.Char8 qualified as BS
import Data.Vector.Unboxed qualified as VU
import Util

modInt :: Int -> Mint.ModInt998244353
modInt = Mint.new

-- verification-helper: PROBLEM https://atcoder.jp/contests/practice2/tasks/practice2_f
main :: IO ()
main = do
  _ <- BS.getLine
  a <- VU.map modInt <$> ints
  b <- VU.map modInt <$> ints
  let c = C.convolution a b
  printBSB $ unwordsBSB $ VU.map fromIntegral c
