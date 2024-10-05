{-# LANGUAGE NumDecimals #-}

import AtCoder.MinCostFlow qualified as MCF
import Control.Monad (unless)
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Char8 qualified as BS
import Data.Foldable
import Data.List qualified as L
import Data.Vector qualified as V
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import Util

-- verification-helper: PROBLEM https://atcoder.jp/contests/practice2/tasks/practice2_e
main :: IO ()
main = do
  (!n, !k) <- ints2
  grid <- V.replicateM n ints

  let [!row0, !col0, !s, !t, !nVerts] = L.scanl' (+) (0 :: Int) [n, n, 1, 1]
  graph <- MCF.new nVerts

  -- bypass (s -> t)
  let big = 10e+9 :: Int
  MCF.addEdge_ graph s t (n * k) big

  -- s -> row, col -> t
  forM_ [0 .. n - 1] $ \i -> do
    -- addEdge graph from to capacity cost
    MCF.addEdge_ graph s (row0 + i) k (0 :: Int)
    MCF.addEdge_ graph (col0 + i) t k (0 :: Int)

  --  row -> col
  for_ [0 .. n - 1] $ \iRow -> do
    for_ [0 .. n - 1] $ \iCol -> do
      let a = grid VG.! iRow VG.! iCol
      MCF.addEdge_ graph (row0 + iRow) (col0 + iCol) (1 :: Int) (big - a)

  (!_, !result) <- MCF.flow graph s t (n * k)
  edges <- MCF.unsafeFreezeEdges graph

  outGrid <- V.replicateM n (VUM.replicate n '.')
  VU.forM_ edges $ \(!from, !to, !flow, !_, !_) -> do
    unless (from == s || to == t || flow == 0) $ do
      VGM.write (outGrid VG.! (from - row0)) (to - col0) 'X'

  printBSB . BSB.intDec $ n * k * big - result
  outGrid' <- V.mapM VU.unsafeFreeze outGrid
  printBSB $ unlinesWithBSB (BSB.byteString . BS.pack . VU.toList) outGrid'
