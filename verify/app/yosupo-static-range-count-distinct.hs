import AtCoder.Extra.Bisect
import AtCoder.Extra.WaveletMatrix qualified as WM
import Control.Monad (unless)
import Data.Maybe (fromJust)
import Data.Vector.Algorithms.Intro qualified as VAI
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import Util

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/static_range_count_distinct
-- #wavelet-matrix
main :: IO ()
main = do
  (!n, !q) <- ints2
  if n == 0
    then do
      printBSB $ unlinesBSB (VU.replicate q (0 :: Int))
    else do
      xs <- ints
      lrs <- VU.replicateM q ints2

      -- In order to count the number of distinct values, focus on the duplicates instead.
      -- Store the number of duplicates in the wavelet matrix. Say we're interested in a specific `y`:
      --
      --    xs      y  .  .  y  .  y  .  y  -- maps the index i to some value
      --    i       0  1  2  3  4  5  6  7
      --    xs'     -        0     3     5  -- maps the index i to the index of the last occurence of the same value
      --                     [-----]        in the interval [3, 5], the number of `y` \in [3, 5] is calculated with `rankBetween [3, 5] [3, 5)`, which evaluates to 1 (duplicates: 1)
      --                     [-----------]  in the interval [3, 7], the number of `y` \in [3, 7] is calculated with `rankBetween [3, 7] [3, 7)`, which evaluates to 2 (duplicates: 1)
      let xs' = VU.create $ do
            vec <- VUM.replicate n (-1 :: Int)
            let !dict = VU.uniq $ VU.modify (VAI.sortBy compare) xs
            lastIndex <- VUM.replicate (VG.length dict) (-1 :: Int)
            VU.iforM_ (VU.map (lowerBound dict) xs) $ \i x -> do
              iLast <- VGM.exchange lastIndex x i
              unless (iLast == -1) $ do
                VGM.write vec i iLast
            return vec

      let wm = WM.build xs'

      -- Count the number of duplicates and subtract it from each interval length:
      -- let res = VU.map (\(!l, !r) -> (r - l) - WM.rankBetween wm l r l r) lrs

      -- Count the number of distinct values directly:
      let res = VU.map (\(!l, !r) -> WM.rankBetween wm l r (-1) l) lrs

      printBSB $ unlinesBSB res
