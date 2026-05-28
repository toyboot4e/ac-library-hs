import Data.ByteString.Char8 qualified as BS
import Data.Maybe (fromJust)
import Data.Vector qualified as V

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/addition_of_big_integers
main :: IO ()
main = do
  (!t, !_) <- fromJust . BS.readInt <$> BS.getLine
  abs <- V.replicateM t $ do
    s <- BS.getLine
    let (!a, !s') = fromJust $ BS.readInteger s
    let (!b, !_) = fromJust . BS.readInteger $ BS.dropSpace s'
    pure $! a + b
  V.forM_ abs print
