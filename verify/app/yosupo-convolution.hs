import AtCoder.Convolution qualified as C
import Data.ByteString.Char8 qualified as BS
import Data.Proxy (Proxy (..))
import Data.Vector.Unboxed qualified as VU
import Util

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/convolution_mod
main :: IO ()
main = do
  _ <- BS.getLine
  a <- ints
  b <- ints
  let c = C.convolutionRaw (Proxy @998244353) a b
  printBSB $ unwordsBSB $ VU.map fromIntegral c
