import AtCoder.Extra.Semigroup.Matrix qualified as Mat
import AtCoder.ModInt qualified as M
import Control.Monad.Trans.State.Strict (evalStateT)
import Data.ByteString.Char8 qualified as BS
import Data.Maybe (fromJust)
import Data.Vector.Unboxed qualified as VU
import Util

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/inverse_matrix
main :: IO ()
main = do
  n <- int
  rest <- BS.getContents
  let mat = fromJust . (`evalStateT` rest) $ do
        vec <- VU.replicateM (n * n) (M.unsafeNew @998244353 . fromIntegral <$> intP)
        pure $ Mat.square n vec

  -- the case of h == 0 or w == 0 is correctly handled under the hood
  case Mat.invRaw mat of
    Just (!_, !mat') -> do
      printBSB $ unlinesWithBSB (unwordsBSB . VU.map M.val) mat'
    Nothing -> do
      putStrLn "-1"
