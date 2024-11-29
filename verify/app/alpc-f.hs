{-# LANGUAGE DataKinds #-}

import AtCoder.Convolution qualified as C
import Data.ByteString.Char8 qualified as BS
import Data.Proxy (Proxy (..))
import Data.Vector.Unboxed qualified as VU
import Util

-- verification-helper: PROBLEM https://atcoder.jp/contests/practice2/tasks/practice2_f
main :: IO ()
main = do
  _ <- BS.getLine
  a <- ints
  b <- ints
  let c = C.convolutionMod (Proxy @998244353) a b
  printBSB $ unwordsBSB $ VU.map fromIntegral c
