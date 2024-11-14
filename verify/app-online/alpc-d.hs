{-# LANGUAGE LambdaCase #-}

import AtCoder.MaxFlow qualified as MF
import Control.Monad (unless, when)
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Char8 qualified as BS
import Data.Foldable
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Util

-- verification-helper: PROBLEM https://atcoder.jp/contests/practice2/tasks/practice2_d
main :: IO ()
main = do
  (!h, !w) <- ints2
  grid <- V.replicateM h BS.getLine

  graph <- MF.new (h * w + 2)
  let s = h * w
  let t = h * w + 1

  -- s -> even, odd -> t
  for_ [0 .. h - 1] $ \y -> do
    for_ [0 .. w - 1] $ \x -> do
      unless (BS.index (grid VG.! y) x == '#') $ do
        let v = w * y + x
        if even (y + x)
          then MF.addEdge_ graph s v (1 :: Int)
          else MF.addEdge_ graph v t (1 :: Int)

  -- even -> odd
  for_ [0 .. h - 1] $ \y -> do
    for_ [0 .. w - 1] $ \x -> do
      when (even (y + x) && BS.index (grid VG.! y) x == '.') $ do
        let v = w * y + x
        for_ [(y - 1, x), (y + 1, x), (y, x - 1), (y, x + 1)] $ \(!y', !x') -> do
          when (0 <= y' && y' < h && 0 <= x' && x' < w && BS.index (grid VG.! y') x' == '.') $ do
            let v' = w * y' + x'
            MF.addEdge_ graph v v' (1 :: Int)
          pure ()

  maxFlow <- MF.flow graph s t
  printBSB $ BSB.intDec maxFlow

  edges <- MF.edges graph
  outGrid <- V.mapM (VU.unsafeThaw . VU.fromList . BS.unpack) grid
  VU.forM_ edges $ \(!from, !to, !_, !flow) -> do
    when (from /= s && to /= t && flow /= 0) $ do
      let (!y, !x) = from `divMod` w
      let (!y', !x') = to `divMod` w
      case (compare y y', compare x x') of
        (GT, EQ) -> do
          VGM.write (outGrid VG.! (y - 1)) x 'v'
          VGM.write (outGrid VG.! (y + 0)) x '^'
        (LT, EQ) -> do
          VGM.write (outGrid VG.! (y + 0)) x 'v'
          VGM.write (outGrid VG.! (y + 1)) x '^'
        (EQ, GT) -> do
          VGM.write (outGrid VG.! y) (x + 0) '<'
          VGM.write (outGrid VG.! y) (x - 1) '>'
        (EQ, LT) -> do
          VGM.write (outGrid VG.! y) (x + 1) '<'
          VGM.write (outGrid VG.! y) (x + 0) '>'
        _ -> error "unreachable"

  outGrid' <- V.mapM VU.unsafeFreeze outGrid
  printBSB $ unlinesWithBSB (BSB.byteString . BS.pack . VU.toList) outGrid'

