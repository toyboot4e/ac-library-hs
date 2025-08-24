import AtCoder.Extra.AhoCorasick qualified as Ac
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Builder qualified as BSB
import Data.Char (ord)
import Data.Foldable (for_)
import Data.Vector qualified as V
import Data.Vector.Generic qualified as VG
import Data.Vector.Unboxed qualified as VU
import Util

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/aho_corasick
main :: IO ()
main = do
  n <- int
  ss <- V.replicateM n (VU.map (subtract (ord 'a') . ord) . VU.fromList . BS.unpack <$> BS.getLine)

  let !ac = Ac.build ss
  let !size = Ac.sizeAc ac
  printBSB $ BSB.intDec size

  for_ [1 .. size - 1] $ \i -> do
    printBSB $ show2 (Ac.parentAc ac VG.! i, Ac.suffixAc ac VG.! i)

  let !nodes = V.map (Ac.nextN ac 0) ss
  printBSB $ unlinesBSB nodes
