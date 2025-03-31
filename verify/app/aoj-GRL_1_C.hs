import AtCoder.Extra.Graph qualified as Gr
import Data.ByteString.Builder qualified as BSB
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import Util

-- verification-helper: PROBLEM https://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=GRL_1_C
main :: IO ()
main = do
  (!n, !m) <- ints2
  uvws <- VU.replicateM m ints3

  let !undefW = maxBound `div` 2 :: Int
  let mat = Gr.floydWarshall n uvws undefW
  if VU.any (\i -> mat VG.! (n * i + i)) $ VU.generate n id
    then putStrLn "NEGATIVE CYCLE"
    else do
      printBSB
        $ unlinesWithBSB
          ( unwordsWithBSB
              ( \x ->
                  if x == undefW
                    then BSB.string7 "INF"
                    else BSB.intDec x
              )
          )
        $ V.unfoldrExactN n (VU.splitAt n) mat
