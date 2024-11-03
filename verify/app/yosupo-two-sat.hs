import AtCoder.TwoSat qualified as TS
import Data.Bit (Bit (..))
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Char8 qualified as BS
import Data.Vector.Unboxed qualified as VU
import Util

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/two_sat
main :: IO ()
main = do
  [!_, !_, !n_, !m_] <- BS.words <$> BS.getLine
  let Just (!n, !_) = BS.readInt n_
  let Just (!m, !_) = BS.readInt m_
  (!xs, !ys, !_) <- VU.unzip3 <$> VU.replicateM m ints3

  ts <- TS.new (n + 1)
  VU.forM_ (VU.zip xs ys) $ \(!x, !y) -> do
    let (!v1, !b1) = if x > 0 then (x, True) else (-x, False)
    let (!v2, !b2) = if y > 0 then (y, True) else (-y, False)
    TS.addClause ts v1 b1 v2 b2

  b <- TS.satisfiable ts
  if b
    then do
      printBSB $ BSB.string8 "s SATISFIABLE"
      putBSB $ BSB.string8 "v "
      answer <- TS.answer ts
      putBSB
        . unwordsBSB
        . VU.tail
        $ VU.imap
          ( \i (Bit ans) -> do
              if ans
                then i
                else -i
          )
          answer
      printBSB $ BSB.string8 " 0"
    else do
      printBSB $ BSB.string8 "s UNSATISFIABLE"
