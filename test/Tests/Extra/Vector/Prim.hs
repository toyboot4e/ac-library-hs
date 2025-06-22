{-# LANGUAGE ViewPatterns #-}

module Tests.Extra.Vector.Prim where

import AtCoder.Extra.Vector.Prim qualified as EV
import Control.Monad.ST (ST, runST)
import Data.Vector.Generic qualified as VG
import Data.Vector.Unboxed qualified as VU
import Test.Tasty
import Test.Tasty.QuickCheck as QC

prop_replicateM :: Int -> Int -> QC.Property
prop_replicateM n x = VU.replicate n x QC.=== runST (EV.replicateM n (pure x))

prop_generateM :: Int -> Int -> QC.Property
prop_generateM n x = VU.generate n (+ x) QC.=== runST (EV.generateM n (pure . (+ x)))

prop_iterateNM :: Int -> Int -> QC.Property
prop_iterateNM n x = VU.iterateN n (* 2) x QC.=== runST (EV.iterateNM n (pure . (* 2)) x)

prop_constructNM :: QC.NonNegative Int -> QC.Property
prop_constructNM (QC.NonNegative n) = VU.constructN n VG.length QC.=== runST (EV.constructNM n (pure . VG.length))

prop_constructrNM :: QC.NonNegative Int -> QC.Property
prop_constructrNM (QC.NonNegative n) = VU.constructrN n VG.length QC.=== runST (EV.constructrNM n (pure . VG.length))

prop_mapM :: [Int] -> QC.Property
prop_mapM (VU.fromList -> xs) = VU.map (* 2) xs QC.=== runST (EV.mapM (pure . (* 2)) xs)

-- prop_mapM_ :: [Int] -> QC.Property
-- prop_mapM_ xs =

prop_imapM :: [Int] -> QC.Property
prop_imapM (VU.fromList -> xs) = VU.imap (\i x -> 2 * (i + x)) xs QC.=== runST (EV.imapM (\i x -> pure (2 * (i + x))) xs)

-- prop_imapM_ :: [Int] -> QC.Property
-- prop_imapM_ xs =

prop_iforM :: [Int] -> QC.Property
prop_iforM (VU.fromList -> xs) = VU.imap (\i x -> 2 * (i + x)) xs QC.=== runST (EV.iforM xs (\i x -> pure (2 * (i + x))))

-- prop_iforM_ :: [Int] -> QC.Property
-- prop_iforM_ xs =

prop_zipWithM :: [Int] -> [Int] -> QC.Property
prop_zipWithM (VU.fromList -> xs) (VU.fromList -> ys) =
  VU.zipWith (*) xs ys QC.=== runST (EV.zipWithM (\x y -> pure (x * y)) xs ys)

-- prop_zipWithM_ :: [Int] -> [Int] -> QC.Property
-- prop_zipWithM_  (VU.fromList -> xs) (VU.fromList -> ys) =

prop_izipWithM :: [Int] -> [Int] -> QC.Property
prop_izipWithM (VU.fromList -> xs) (VU.fromList -> ys) =
  VU.izipWith (\i a b -> i + a * b) xs ys QC.=== runST (EV.izipWithM (\i a b -> pure (i + a * b)) xs ys)

-- prop_izipWithM_ :: [Int] -> [Int] -> QC.Property
-- prop_izipWithM_  (VU.fromList -> xs) (VU.fromList -> ys) =

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

prop_filterM :: [Int] -> QC.Property
prop_filterM (VU.fromList -> xs) = VU.filter odd xs QC.=== runST (EV.filterM (pure . odd) xs)

prop_mapMaybeM :: [Int] -> QC.Property
prop_mapMaybeM (VU.fromList -> xs) =
  let f x = if odd x then Just (2 * x) else Nothing
   in VU.mapMaybe f xs QC.=== runST (EV.mapMaybeM (pure . f) xs)

prop_imapMaybeM :: [Int] -> QC.Property
prop_imapMaybeM (VU.fromList -> xs) =
  let f i x = if odd i then Just (i * x) else Nothing
   in VU.imapMaybe f xs QC.=== runST (EV.imapMaybeM (\i -> pure . f i) xs)

-- | scanM etc.
prop_monadicScanlLike ::
  ((Int -> Int -> Int) -> Int -> VU.Vector Int -> VU.Vector Int) ->
  (forall s. (Int -> Int -> ST s Int) -> Int -> VU.Vector Int -> ST s (VU.Vector Int)) ->
  Int ->
  [Int] ->
  QC.Property
prop_monadicScanlLike ref acl x xs =
  let xs' = VU.fromList xs
      f = (+)
      mf x y = pure $ x + y
   in ref f x xs' QC.=== runST (acl mf x xs')

-- | scanM1 etc.
prop_monadicScanl1Like ::
  ((Int -> Int -> Int) -> VU.Vector Int -> VU.Vector Int) ->
  (forall s. (Int -> Int -> ST s Int) -> VU.Vector Int -> ST s (VU.Vector Int)) ->
  QC.NonEmptyList Int ->
  QC.Property
prop_monadicScanl1Like ref acl (QC.NonEmpty xs) =
  let xs' = VU.fromList xs
      f = (+)
      mf x y = pure $ x + y
   in ref f xs' QC.=== runST (acl mf xs')

tests :: [TestTree]
tests =
  [ QC.testProperty "replicateM" prop_replicateM,
    QC.testProperty "generateM" prop_generateM,
    QC.testProperty "iterateNM" prop_iterateNM,
    QC.testProperty "constructNM" prop_constructNM,
    QC.testProperty "constructrNM" prop_constructrNM,
    QC.testProperty "mapM" prop_mapM,
    -- QC.testProperty "mapM_" prop_mapM_,
    QC.testProperty "imapM" prop_imapM,
    -- QC.testProperty "imapM_" prop_imapM_,
    QC.testProperty "iforM" prop_iforM,
    -- QC.testProperty "iforM_" prop_iforM_,
    QC.testProperty "zipWithM" prop_zipWithM,
    -- QC.testProperty "zipWithM_" prop_zipWithM_,
    QC.testProperty "izipWithM" prop_izipWithM,
    -- QC.testProperty "izipWithM_" prop_izipWithM_,
    QC.testProperty "concatMapM" prop_concatMapM,
    QC.testProperty "iconcatMap" prop_iconcatMap,
    QC.testProperty "iconcatMapM" prop_iconcatMapM,
    QC.testProperty "filterM" prop_filterM,
    QC.testProperty "mapMaybeM" prop_mapMaybeM,
    QC.testProperty "imapMaybeM" prop_imapMaybeM,
    QC.testProperty "prescanlM" (prop_monadicScanlLike VU.prescanl EV.prescanlM),
    QC.testProperty "prescanlM'" (prop_monadicScanlLike VU.prescanl' EV.prescanlM'),
    QC.testProperty "postscanlM" (prop_monadicScanlLike VU.postscanl EV.postscanlM),
    QC.testProperty "postscanlM'" (prop_monadicScanlLike VU.postscanl' EV.postscanlM'),
    QC.testProperty "scanlM" (prop_monadicScanlLike VU.scanl EV.scanlM),
    QC.testProperty "scanlM'" (prop_monadicScanlLike VU.scanl' EV.scanlM'),
    QC.testProperty "scanl1M" (prop_monadicScanl1Like VU.scanl1 EV.scanl1M),
    QC.testProperty "scanl1M'" (prop_monadicScanl1Like VU.scanl1' EV.scanl1M')
  ]
