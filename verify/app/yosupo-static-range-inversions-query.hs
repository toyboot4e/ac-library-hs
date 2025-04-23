{-# LANGUAGE ViewPatterns #-}

import AtCoder.Extra.Bisect
import AtCoder.Extra.Mo qualified as Mo
import AtCoder.FenwickTree qualified as Ft
import Control.Monad.State.Class
import Control.Monad.Trans.State.Strict (evalStateT)
import Data.Vector.Algorithms.Intro qualified as VAI
import Data.Vector.Generic qualified as VG
import Data.Vector.Unboxed qualified as VU
import Util

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/static_range_inversions_query
main :: IO ()
main = do
  (!n, !q) <- ints2
  xs0 <- ints
  lrs <- VU.replicateM q ints2

  let !dict = VU.uniq $ VU.modify VAI.sort xs0
  let !xs = VU.map (lowerBound dict) xs0

  let !len = VU.length dict
  ft <- Ft.new len
  let addL ((xs VG.!) -> x) = do
        -- count the numbers less than `x`
        dn <- Ft.sum ft 0 x
        modify' (+ dn)
        Ft.add ft x 1

  let addR ((xs VG.!) -> x) = do
        -- count the numbers bigger than than `x`
        dn <- Ft.sum ft (x + 1) len
        modify' (+ dn)
        Ft.add ft x 1

  let delL ((xs VG.!) -> x) = do
        -- count the numbers less than `x`
        dn <- Ft.sum ft 0 x
        modify' (subtract dn)
        Ft.add ft x (-1)

  let delR ((xs VG.!) -> x) = do
        -- count the numbers bigger than than `x`
        dn <- Ft.sum ft (x + 1) len
        modify' (subtract dn)
        Ft.add ft x (-1)

  let query _ = get

  result <- evalStateT (Mo.run n lrs addL addR delL delR query) (0 :: Int)
  printBSB $ unlinesBSB result
