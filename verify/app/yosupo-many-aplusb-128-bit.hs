import Control.Monad (replicateM_)
import Data.ByteString.Char8 qualified as BS
import Data.WideWord.Int128 (Int128)
import Util

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/many_aplusb_128bit
main :: IO ()
main = do
  t <- int
  replicateM_ t $ do
    [!a, !b] :: [Int128] <- map (read . BS.unpack) . BS.words <$> BS.getLine
    print $! a + b
