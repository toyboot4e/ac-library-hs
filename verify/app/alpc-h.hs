import AtCoder.TwoSat qualified as TS
import Control.Monad (unless, when)
import Data.Bit (Bit (..))
import Data.ByteString.Builder qualified as BSB
import Data.Foldable
import Data.Vector.Generic qualified as VG
import Data.Vector.Unboxed qualified as VU
import System.Exit (exitSuccess)
import Util

-- verification-helper: PROBLEM https://atcoder.jp/contests/practice2/tasks/practice2_h
main :: IO ()
main = do
  (!n, !d) <- ints2
  (!xs, !ys) <- VU.unzip <$> VU.replicateM n ints2

  ts <- TS.new n
  for_ [0 .. n - 1] $ \i -> do
    for_ [i + 1 .. n - 1] $ \j -> do
      when (abs (xs VG.! i - xs VG.! j) < d) $ do
        -- cannot use both of x[i] and x[j]
        TS.addClause ts i False j False
      when (abs (xs VG.! i - ys VG.! j) < d) $ do
        TS.addClause ts i False j True
      when (abs (ys VG.! i - xs VG.! j) < d) $ do
        TS.addClause ts i True j False
      when (abs (ys VG.! i - ys VG.! j) < d) $ do
        TS.addClause ts i True j True
      return ()

  b <- TS.satisfiable ts
  unless b $ do
    printBSB $ BSB.string8 "No"
    exitSuccess

  printBSB $ BSB.string8 "Yes"
  answer <- TS.answer ts
  printBSB . unlinesBSB $
    VU.imap
      ( \i (Bit ans) -> do
          if ans
            then xs VG.! i
            else ys VG.! i
      )
      answer
