module Tests.Extra.Bisect where

import AtCoder.Extra.Bisect
import Data.List qualified as L
import Data.Vector.Generic qualified as VG
import Data.Vector.Unboxed qualified as VU
import Test.Tasty
import Test.Tasty.QuickCheck as QC

-- | Takes half-open interval [l, r).
naivePartition :: Int -> Int -> (Int -> Bool) -> VU.Vector Int -> (Maybe Int, Maybe Int)
naivePartition l r p xs
  | l >= r = (Nothing, Nothing)
  | otherwise = case (VU.null ls, VU.null rs) of
      (True, True) -> error "unreachable"
      (False, True) -> (Just l', Nothing)
      (True, False) -> (Nothing, Just r')
      _ -> (Just l', Just r')
  where
    xs' = VU.take (r - l) . VU.drop l $ xs
    (!ls, !rs) = VU.partition p xs'
    l' = l + VU.length ls - 1
    r' = l' + 1

naiveLowerBound :: VU.Vector Int -> Int -> Maybe Int
naiveLowerBound xs = naiveLowerBoundIn 0 (VU.length xs) xs

naiveLowerBoundIn :: Int -> Int -> VU.Vector Int -> Int -> Maybe Int
naiveLowerBoundIn l r xs target = case naivePartition l r (< target) xs of
  (!_, Just i) -> Just i
  _ -> Nothing

naiveUpperBound :: VU.Vector Int -> Int -> Maybe Int
naiveUpperBound xs = naiveUpperBoundIn 0 (VU.length xs) xs

naiveUpperBoundIn :: Int -> Int -> VU.Vector Int -> Int -> Maybe Int
naiveUpperBoundIn l r xs target = case naivePartition l r (<= target) xs of
  (!_, Just i) -> Just i
  _ -> Nothing

boundsQueryGen :: Gen (Int, Int, VU.Vector Int)
boundsQueryGen = do
  n <- QC.chooseInt (1, 100)
  p <- QC.chooseInt (-25, 25)
  xs <- VU.fromList . L.sort <$> QC.vectorOf n (QC.chooseInt (-20, 20))
  pure (n, p, xs)

bisectQueryGen :: Gen (Int, Int, VU.Vector Int, [(Int, Int)])
bisectQueryGen = do
  n <- QC.chooseInt (1, 100)
  p <- QC.chooseInt (-25, 25)
  xs <- VU.fromList . L.sort <$> QC.vectorOf n (QC.chooseInt (-20, 20))
  let lrs = [(l, r) | l <- [0 .. n], r <- [l .. n]]
  pure (n, p, xs, lrs)

prop_lowerBound :: TestTree
prop_lowerBound = QC.testProperty "lowerBound" $ do
  (!_, !target, !xs) <- boundsQueryGen
  pure $ naiveLowerBound xs target QC.=== lowerBound xs target

prop_lowerBoundIn :: TestTree
prop_lowerBoundIn = QC.testProperty "lowerBoundIn" $ do
  (!_, !target, !xs, !lrs) <- bisectQueryGen
  pure . QC.conjoin $
    map
      ( \(!l, !r) ->
          naiveLowerBoundIn l r xs target == lowerBoundIn l r xs target
      )
      lrs

prop_upperBound :: TestTree
prop_upperBound = QC.testProperty "upperBound" $ do
  (!_, !target, !xs) <- boundsQueryGen
  pure $ naiveUpperBound xs target QC.=== upperBound xs target

prop_upperBoundIn :: TestTree
prop_upperBoundIn = QC.testProperty "upperBoundIn" $ do
  (!_, !target, !xs, !lrs) <- bisectQueryGen
  pure . QC.conjoin $
    map
      ( \(!l, !r) ->
          naiveUpperBoundIn l r xs target == upperBoundIn l r xs target
      )
      lrs

prop_bisectL :: TestTree
prop_bisectL = QC.testProperty "bisectL" $ do
  (!_, !boundary, !xs, !lrs) <- bisectQueryGen
  pure . QC.conjoin $
    map
      ( \(!l, !r) ->
          fst (naivePartition l r (<= boundary) xs) == bisectL l r (\i -> xs VG.! i <= boundary)
      )
      lrs

prop_bisectR :: TestTree
prop_bisectR = QC.testProperty "bisectR" $ do
  (!_, !boundary, !xs, !lrs) <- bisectQueryGen
  pure . QC.conjoin $
    map
      ( \(!l, !r) ->
          snd (naivePartition l r (<= boundary) xs) == bisectR l r (\i -> xs VG.! i <= boundary)
      )
      lrs

tests :: [TestTree]
tests =
  [ prop_lowerBound,
    prop_upperBound,
    prop_lowerBoundIn,
    prop_upperBoundIn,
    prop_bisectL,
    prop_bisectR
  ]
