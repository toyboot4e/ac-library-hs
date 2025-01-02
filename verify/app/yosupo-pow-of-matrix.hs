import AtCoder.Extra.Semigroup.Matrix qualified as Mat
import Control.Monad.Trans.State.Strict (evalStateT)
import Data.ByteString.Char8 qualified as BS
import Data.Maybe (fromJust)
import Data.Vector.Unboxed qualified as VU
import Util

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/pow_of_matrix
main :: IO ()
main = do
  (!n, !k) <- ints2
  rest <- BS.getContents
  let !mat = fromJust . (`evalStateT` rest) $ do
        vecA <- VU.replicateM (n * n) intP
        pure $ Mat.new n n vecA
  let !matK = Mat.powMod 998244353 k mat
  printBSB $ showMat matK
