import AtCoder.Extra.Bisect (lowerBound)
import AtCoder.Extra.IntervalMap qualified as ITM
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Builder qualified as BSB
import Data.Maybe (fromJust)
import Data.Vector.Algorithms.Intro qualified as VAI
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import Util

-- verification-helper: PROBLEM https://atcoder.jp/contests/past202104-open/tasks/past202104_m
main :: IO ()
main = do
  n <- int
  xs <- ints
  q <- int
  lrxs <- VU.replicateM q $ withLine ((,,) <$> intS1P <*> intP <*> intP)

  let !dict = VU.modify VAI.sort $ xs VU.++ VU.map (\(!_, !_, !x) -> x) lrxs
  intervals <- ITM.new @_ @Int n
  res <- VUM.replicate 1 (0 :: Int)
  cnt <- VUM.replicate (VG.length dict) (0 :: Int)

  let onAdd l r x = do
        let !i = fromJust $ lowerBound dict x
        let !len = r - l
        VGM.read cnt i >>= \n' -> VGM.modify res (subtract (n' * (n' - 1) `div` 2)) 0
        VGM.modify cnt (+ len) i
        VGM.read cnt i >>= \n' -> VGM.modify res (+ (n' * pred n' `div` 2)) 0

  let onDel l r x = do
        let !i = fromJust $ lowerBound dict x
        let !len = r - l
        VGM.read cnt i >>= \n' -> VGM.modify res (subtract (n' * (n' - 1) `div` 2)) 0
        VGM.modify cnt (subtract len) i
        VGM.read cnt i >>= \n' -> VGM.modify res (+ (n' * pred n' `div` 2)) 0

  intervals <- ITM.buildM xs onAdd
  VU.forM_ lrxs $ \(!l, !r, !x) -> do
    ITM.insertM intervals l r x onAdd onDel
    printBSB . BSB.intDec =<< VGM.read res 0
