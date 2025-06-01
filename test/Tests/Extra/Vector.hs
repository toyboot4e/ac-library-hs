module Tests.Extra.Vector where

import AtCoder.Extra.Vector qualified as EV
import Control.Monad.ST (runST)
import Data.List qualified as L
import Data.Vector.Unboxed qualified as VU
import Data.Vector qualified as V
import Data.Vector.Generic qualified as VG
import Test.Tasty
import Test.Tasty.QuickCheck as QC

prop_argsort :: [Int] -> QC.Property
prop_argsort xs =
  let lhs = VU.fromList . map snd . L.sort $ zip xs [0 :: Int ..]
      rhs = EV.argsort $ VU.fromList xs
   in lhs QC.=== rhs

prop_concatMapM :: [Int] -> QC.Property
prop_concatMapM xs =
  let f x = VU.fromList [x, x, x]
      vec = VU.fromList xs
      lhs = VU.concatMap f vec
      rhs = runST $ EV.concatMapM (pure . f) vec
   in lhs QC.=== rhs

prop_iconcatMap :: [Int] -> QC.Property
prop_iconcatMap xs =
  let f i x = VU.fromList [i + x, i + x, i + x]
      vec = VU.fromList xs
      lhs = VU.concat $ zipWith f [0 :: Int ..] xs
      rhs = runST $ EV.iconcatMapM (\i x -> pure (f i x)) vec
   in lhs QC.=== rhs

prop_iconcatMapM :: [Int] -> QC.Property
prop_iconcatMapM xs =
  let f i x = VU.fromList [i + x, i + x, i + x]
      vec = VU.fromList xs
      lhs = VU.concat $ zipWith f [0 :: Int ..] xs
      rhs = runST $ EV.iconcatMapM (\i x -> pure (f i x)) vec
   in lhs QC.=== rhs

prop_chunks :: QC.Positive Int -> [Int] -> QC.Property
prop_chunks (QC.Positive k) [] = EV.chunks k (VU.empty @Int) QC.=== V.empty
prop_chunks (QC.Positive k) xs =
  let res = EV.chunks k $ VU.fromList xs
      n = length xs
      lastLen = VG.length (V.last res)
   in QC.conjoin
      [ V.sum (VG.map VG.length res) QC.=== n,
        V.all ((== k) . VG.length) (V.init res) QC.=== True,
        VG.concat (V.toList res) QC.=== VU.fromList xs
      ]

prop_maxRangeSum :: [Int] -> QC.Property
prop_maxRangeSum xs =
  let vec = VU.fromList xs
      lhs =
        let n = VU.length vec
            lrs = [(l, r) | l <- [0 .. n], r <- [l .. n]]
            eval (!l, !r) = VU.sum . VU.take (r - l) $ VU.drop l vec
         in maximum $ map eval lrs
      rhs = EV.maxRangeSum vec
   in lhs QC.=== rhs

prop_minRangeSum :: [Int] -> QC.Property
prop_minRangeSum xs =
  let vec = VU.fromList xs
      lhs =
        let n = VU.length vec
            lrs = [(l, r) | l <- [0 .. n], r <- [l .. n]]
            eval (!l, !r) = VU.sum . VU.take (r - l) $ VU.drop l vec
         in minimum $ map eval lrs
      rhs = EV.minRangeSum vec
   in lhs QC.=== rhs

tests :: [TestTree]
tests =
  [ QC.testProperty "argsort" prop_argsort,
    QC.testProperty "concatMapM" prop_concatMapM,
    QC.testProperty "iconcatMap" prop_iconcatMap,
    QC.testProperty "maxRangeSum" prop_maxRangeSum,
    QC.testProperty "minRangeSum" prop_minRangeSum,
    QC.testProperty "iconcatMapM" prop_iconcatMapM,
    QC.testProperty "chunks" prop_chunks,
    QC.testProperty "maxRangeSum" prop_maxRangeSum,
    QC.testProperty "minRangeSum" prop_minRangeSum
  ]

