import AtCoder.Extra.Semigroup.Matrix qualified as Mat
import Control.Monad.Trans.State.Strict (evalStateT)
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Char8 qualified as BS
import Data.Maybe (fromJust)
import Data.Vector.Unboxed qualified as VU
import Util

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/matrix_det_arbitrary_mod
main :: IO ()
main = do
  (!n, !m) <- ints2
  rest <- BS.getContents
  let mat = fromJust . (`evalStateT` rest) $ do
        vec <- VU.replicateM (n * n) intP
        pure $ Mat.square n vec

  printBSB . BSB.intDec $ Mat.detMod m mat
