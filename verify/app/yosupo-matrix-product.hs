import AtCoder.Extra.Semigroup.Matrix qualified as Mat
import AtCoder.ModInt qualified as M
import Control.Monad.Trans.State.Strict (evalStateT)
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Char8 qualified as BS
import Data.Maybe (fromJust)
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import Util

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/matrix_product
main :: IO ()
main = do
  (!n, !m, !k) <- ints3
  rest <- BS.getContents
  let (!matA, !matB) = fromJust . (`evalStateT` rest) $ do
        vecA <- VU.replicateM (n * m) intP
        vecB <- VU.replicateM (m * k) intP
        pure (Mat.new n m vecA, Mat.new m k vecB)
  let !matC = Mat.mulMod 998244353 matA matB
  printBSB $ showMat matC
