module Tests.Extra.Vector where

import AtCoder.Extra.Vector qualified as EV
import Control.Monad.ST (runST)
import Data.Functor.Identity (Identity, runIdentity)
import Data.List qualified as L
import Data.Vector qualified as V
import Data.Vector.Generic qualified as VG
import Data.Vector.Unboxed qualified as VU
import Test.Tasty
import Test.Tasty.HUnit
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

prop_mapAccumL :: [Int] -> QC.Property
prop_mapAccumL xs =
  let f s x = (s * x, s + x)
      (!l1, !l2) = L.mapAccumL f (0 :: Int) xs
      (!r1, !r2) = EV.mapAccumL f (0 :: Int) $ VU.fromList xs
   in QC.conjoin [l1 QC.=== r1, VU.fromList l2 QC.=== r2]

-- | scanM etc.
prop_monadicScanlLike ::
  ((Int -> Int -> Int) -> Int -> VU.Vector Int -> VU.Vector Int) ->
  ((Int -> Int -> Identity Int) -> Int -> VU.Vector Int -> Identity (VU.Vector Int)) ->
  Int ->
  [Int] ->
  QC.Property
prop_monadicScanlLike ref acl x xs =
  let xs' = VU.fromList xs
      f = (+)
      mf x y = pure $ x + y
   in ref f x xs' QC.=== runIdentity (acl mf x xs')

-- | scanM1 etc.
prop_monadicScanl1Like ::
  ((Int -> Int -> Int) -> VU.Vector Int -> VU.Vector Int) ->
  ((Int -> Int -> Identity Int) -> VU.Vector Int -> Identity (VU.Vector Int)) ->
  QC.NonEmptyList Int ->
  QC.Property
prop_monadicScanl1Like ref acl (QC.NonEmpty xs) =
  let xs' = VU.fromList xs
      f = (+)
      mf x y = pure $ x + y
   in ref f xs' QC.=== runIdentity (acl mf xs')

prop_chunks :: QC.Positive Int -> [Int] -> QC.Property
prop_chunks (QC.Positive k) [] = EV.chunks k (VU.empty @Int) QC.=== V.empty
prop_chunks (QC.Positive k) xs =
  let res = EV.chunks k $ VU.fromList xs
      n = length xs
   in QC.conjoin
        [ V.sum (VG.map VG.length res) QC.=== n,
          V.all ((== k) . VG.length) (V.init res) QC.=== True,
          VG.concat (V.toList res) QC.=== VU.fromList xs
        ]

unit_maxRangeSum :: TestTree
unit_maxRangeSum = testCase "unit_maxRangeSum" $ do
  EV.maxRangeSum (VU.singleton (-1 :: Int)) @?= 0
  EV.maxRangeSum (VU.empty @Int) @?= 0

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

unit_minRangeSum :: TestTree
unit_minRangeSum = testCase "unit_minRangeSum [1]" $ do
  EV.minRangeSum (VU.singleton (1 :: Int)) @?= 0
  EV.minRangeSum (VU.empty @Int) @?= 0

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

prop_slideMinIndices :: QC.Positive Int -> [Int] -> QC.Property
prop_slideMinIndices (QC.Positive k) xs =
  let vec = VU.fromList xs
   in slideMin k vec QC.=== EV.slideMinIndices k vec

prop_slideMaxIndices :: QC.Positive Int -> [Int] -> QC.Property
prop_slideMaxIndices (QC.Positive k) xs =
  let vec = VU.fromList xs
   in slideMax k vec QC.=== EV.slideMaxIndices k vec

slideMin :: Int -> VU.Vector Int -> VU.Vector Int
slideMin k xs
  | VU.null xs = VU.empty
  | k >= n = VU.singleton $ VU.minIndex xs
  | otherwise = VU.generate (n - (k - 1)) $ \l ->
      let slice = VU.take k $ VU.drop l xs
       in (+ l) $ VU.minIndex slice
  where
    n = VU.length xs

slideMax :: Int -> VU.Vector Int -> VU.Vector Int
slideMax k xs
  | VU.null xs = VU.empty
  | k >= n = VU.singleton $ VU.maxIndex xs
  | otherwise = VU.generate (n - (k - 1)) $ \l ->
      let slice = VU.take k $ VU.drop l xs
       in (+ l) $ VU.maxIndex slice
  where
    n = VU.length xs

tests :: [TestTree]
tests =
  [ QC.testProperty "argsort" prop_argsort,
    QC.testProperty "concatMapM" prop_concatMapM,
    QC.testProperty "iconcatMap" prop_iconcatMap,
    QC.testProperty "iconcatMapM" prop_iconcatMapM,
    QC.testProperty "mapAccumL" prop_mapAccumL,
    QC.testProperty "chunks" prop_chunks,
    QC.testProperty "prescanlM" (prop_monadicScanlLike VU.prescanl EV.prescanlM),
    QC.testProperty "prescanlM'" (prop_monadicScanlLike VU.prescanl' EV.prescanlM'),
    QC.testProperty "postscanlM" (prop_monadicScanlLike VU.postscanl EV.postscanlM),
    QC.testProperty "postscanlM'" (prop_monadicScanlLike VU.postscanl' EV.postscanlM'),
    QC.testProperty "scanlM" (prop_monadicScanlLike VU.scanl EV.scanlM),
    QC.testProperty "scanlM'" (prop_monadicScanlLike VU.scanl' EV.scanlM'),
    QC.testProperty "scanl1M" (prop_monadicScanl1Like VU.scanl1 EV.scanl1M),
    QC.testProperty "scanl1M'" (prop_monadicScanl1Like VU.scanl1' EV.scanl1M'),
    QC.testProperty "maxRangeSum" prop_maxRangeSum,
    QC.testProperty "minRangeSum" prop_minRangeSum,
    unit_maxRangeSum,
    unit_minRangeSum,
    QC.testProperty "slideMinIndices" prop_slideMinIndices,
    QC.testProperty "slideMaxIndices" prop_slideMaxIndices
  ]
