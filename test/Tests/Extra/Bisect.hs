module Tests.Extra.Bisect where

import AtCoder.Extra.Bisect
import Data.List qualified as L
import Data.Vector.Generic qualified as VG
import Data.Vector.Unboxed qualified as VU
import Test.Tasty
import Test.Tasty.QuickCheck as QC

-- | Takes half-open interval [l, r).
naiveMaxRightIn :: Int -> Int -> (Int -> Bool) -> VU.Vector Int -> Int
naiveMaxRightIn l r p xs =
  (+ l)
    . VU.length
    . VU.takeWhile p
    . VU.take (r - l)
    $ VU.drop l xs

naiveLowerBound :: VU.Vector Int -> Int -> Int
naiveLowerBound xs = naiveLowerBoundIn 0 (VU.length xs) xs

naiveLowerBoundIn :: Int -> Int -> VU.Vector Int -> Int -> Int
naiveLowerBoundIn l r xs target = naiveMaxRightIn l r (< target) xs

naiveUpperBound :: VU.Vector Int -> Int -> Int
naiveUpperBound xs = naiveUpperBoundIn 0 (VU.length xs) xs

naiveUpperBoundIn :: Int -> Int -> VU.Vector Int -> Int -> Int
naiveUpperBoundIn l r xs target = naiveMaxRightIn l r (<= target) xs

boundsQueryGen :: Gen (Int, Int, VU.Vector Int)
boundsQueryGen = do
  n <- QC.chooseInt (1, 100)
  p <- QC.chooseInt (-25, 25)
  xs <- VU.fromList . L.sort <$> QC.vectorOf n (QC.chooseInt (-20, 20))
  pure (n, p, xs)

maxRightQueryGen :: Gen (Int, Int, VU.Vector Int, [(Int, Int)])
maxRightQueryGen = do
  n <- QC.chooseInt (1, 100)
  p <- QC.chooseInt (-25, 25)
  xs <- VU.fromList . L.sort <$> QC.vectorOf n (QC.chooseInt (-20, 20))
  let lrs = [(l, r) | l <- [0 .. n], r <- [l .. n]]
  pure (n, p, xs, lrs)

prop_lowerBound :: TestTree
prop_lowerBound = QC.testProperty "lowerBound" $ do
  (!_, !target, !xs) <- boundsQueryGen
  pure . QC.counterexample (show (target, xs)) $ naiveLowerBound xs target QC.=== lowerBound xs target

prop_lowerBoundIn :: TestTree
prop_lowerBoundIn = QC.testProperty "lowerBoundIn" $ do
  (!_, !target, !xs, !lrs) <- maxRightQueryGen
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
  (!_, !target, !xs, !lrs) <- maxRightQueryGen
  pure . QC.conjoin $
    map
      ( \(!l, !r) ->
          naiveUpperBoundIn l r xs target == upperBoundIn l r xs target
      )
      lrs

prop_maxRight :: TestTree
prop_maxRight = QC.testProperty "maxRight" $ do
  (!_, !boundary, !xs, !lrs) <- maxRightQueryGen
  pure . QC.conjoin $
    map
      ( \(!l, !r) ->
          naiveMaxRightIn l r (<= boundary) xs == maxRight l r (\i -> xs VG.! i <= boundary)
      )
      lrs

minLeftQueryGen :: Gen (Int, Int, VU.Vector Int, [(Int, Int)])
minLeftQueryGen = do
  n <- QC.chooseInt (1, 100)
  p <- QC.chooseInt (-25, 25)
  xs <- VU.fromList . L.sort <$> QC.vectorOf n (QC.chooseInt (-20, 20))
  let lrs = [(l, r) | l <- [0 .. n], r <- [l .. n]]
  pure (n, p, xs, lrs)

prop_minLeft :: TestTree
prop_minLeft = QC.testProperty "minLeft" $ do
  (!_, !boundary, !xs, !lrs) <- minLeftQueryGen
  pure . QC.conjoin $
    map
      ( \(!l, !r) ->
          let expected =
                (r -)
                  . VU.length
                  . VU.takeWhile (>= boundary)
                  . VU.reverse
                  . VU.take (r - l)
                  $ VU.drop l xs
              res = minLeft l r (\i -> xs VG.! i >= boundary)
           in QC.counterexample (show ((l, r), boundary, xs)) $
                expected QC.=== res
      )
      lrs

tests :: [TestTree]
tests =
  [ prop_lowerBound,
    prop_upperBound,
    prop_lowerBoundIn,
    prop_upperBoundIn,
    prop_maxRight,
    prop_minLeft
  ]
