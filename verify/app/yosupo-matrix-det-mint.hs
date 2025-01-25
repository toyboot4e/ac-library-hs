import AtCoder.Extra.Semigroup.Matrix qualified as Mat
import AtCoder.ModInt qualified as M
import Control.Monad.Trans.State.Strict (evalStateT)
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Char8 qualified as BS
import Data.Maybe (fromJust)
import Data.Vector.Unboxed qualified as VU
import Util

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/matrix_det
main :: IO ()
main = do
  n <- int
  rest <- BS.getContents
  let mat = fromJust . (`evalStateT` rest) $ do
        vec <- VU.replicateM (n * n) (M.new @998244353 <$> intP)
        pure $ Mat.square n vec

  printBSB . BSB.intDec . M.val $ Mat.detMint mat
