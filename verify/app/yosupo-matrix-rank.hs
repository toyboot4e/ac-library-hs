import AtCoder.Extra.Semigroup.Matrix qualified as Mat
import AtCoder.ModInt qualified as M
import Control.Monad.Trans.State.Strict (evalStateT)
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Char8 qualified as BS
import Data.Maybe (fromJust)
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import Util

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/matrix_rank
main :: IO ()
main = do
  (!h, !w) <- ints2
  rest <- BS.getContents
  let mat = fromJust . (`evalStateT` rest) $ do
        vec <- VU.replicateM (h * w) (M.new @998244353 <$> intP)
        pure $ Mat.new h w vec

  -- the case of h == 0 or w == 0 is correctly handled under the hood
  printBSB . BSB.intDec $ Mat.rank mat
