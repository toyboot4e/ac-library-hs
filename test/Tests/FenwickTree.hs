-- | Fenwick tree tests.
module Tests.FenwickTree (tests) where

import AtCoder.FenwickTree qualified as FT
import Control.Monad.ST (runST)
import Data.Foldable
import Data.Vector.Unboxed qualified as VU
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec
import Test.QuickCheck qualified as QC
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck qualified as QC

-- empty
-- assign

unit_zero :: TestTree
unit_zero = testCase "zero" $ do
  ft <- FT.new @_ @Int 0
  (@?= 0) =<< FT.sum ft 0 0

-- TODO: add modint test

-- FIXME: it's uint test and not works with Int
-- overFlowInt :: TestTree
-- overFlowInt = testCase "overFlowInt" $ do
--   ft <- FT.new @_ @Int 10
--   for_ [0 .. 10 - 1] $ \i -> do
--     FT.add ft i (bit 63 + i)
--   for_ [0 .. 10] $ \i -> do
--     for_ [0 .. 10] $ \j -> do
--       let s = sum [i .. j - 1]
--       let expected = if (j - i) `mod` 2 == 1 then bit 63 + s else s
--       result <- FT.sum ft i j
--       result @?= expected

unit_naive :: TestTree
unit_naive = testCase "naive" $ do
  for_ [0 .. 50] $ \n -> do
    ft <- FT.new @_ @Int n
    for_ [0 .. n - 1] $ \i -> do
      FT.add ft i (i * i)
    for_ [0 .. n] $ \l -> do
      for_ [l .. n] $ \r -> do
        let s = sum [i * i | i <- [l .. r - 1]]
        (@?= s) =<< FT.sum ft l r

-- TODO: smint
-- TODO: mint

unit_sumMaybeBounds :: TestTree
unit_sumMaybeBounds = testCase "sumMaybeBounds" $ do
  ft <- FT.build @_ @Int (VU.generate 4 id)
  (@?= Just 0) =<< FT.sumMaybe ft 0 0
  (@?= Just 3) =<< FT.sumMaybe ft 3 4
  (@?= Just 6) =<< FT.sumMaybe ft 0 4
  (@?= Nothing) =<< FT.sumMaybe ft (-1) 4
  (@?= Nothing) =<< FT.sumMaybe ft 0 5
  (@?= Nothing) =<< FT.sumMaybe ft 0 (-1)
  (@?= Nothing) =<< FT.sumMaybe ft (-1) (-1)
  (@?= Nothing) =<< FT.sumMaybe ft (-1) 0
  (@?= Nothing) =<< FT.sumMaybe ft 4 5
  (@?= Nothing) =<< FT.sumMaybe ft 5 5

spec_invalid :: IO TestTree
spec_invalid = testSpec "invalid" $ do
  it "throws error" $ do
    FT.new @_ @Int (-1) `shouldThrow` anyException
  s <- runIO $ FT.new @_ @Int 10

  it "throws error" $ do
    FT.add s (-1) 0 `shouldThrow` anyException
  it "throws error" $ do
    FT.add s 10 0 `shouldThrow` anyException

  it "throws error" $ do
    FT.sum s (-1) 3 `shouldThrow` anyException
  it "throws error" $ do
    FT.sum s 3 11 `shouldThrow` anyException
  it "throws error" $ do
    FT.sum s 5 3 `shouldThrow` anyException

prop_maxRight :: QC.NonNegative Int -> QC.NonEmptyList (QC.NonNegative Int) -> QC.Gen QC.Property
prop_maxRight (QC.NonNegative xRef) (QC.NonEmpty xs_) = do
  l0 <- QC.chooseInt (0, length xs_)
  let xs = VU.fromList $ map (\(QC.NonNegative x) -> x) xs_
      expected = (l0 +) . VU.length . VU.takeWhile (<= xRef) $ VU.scanl1' (+) $ VU.drop l0 xs
      res = runST $ do
        ft <- FT.build xs
        FT.maxRight ft l0 (<= xRef)
  pure $ expected QC.=== res

prop_minLeft :: QC.NonNegative Int -> QC.NonEmptyList (QC.NonNegative Int) -> QC.Gen QC.Property
prop_minLeft (QC.NonNegative xRef) (QC.NonEmpty xs_) = do
  r0 <- QC.chooseInt (0, length xs_)
  let xs = VU.fromList $ map (\(QC.NonNegative x) -> x) xs_
      expected = (r0 -) . VU.length . VU.takeWhile (<= xRef) $ VU.scanl1' (+) $ VU.reverse $ VU.take r0 xs
      res = runST $ do
        ft <- FT.build xs
        FT.minLeft ft r0 (<= xRef)
  pure $ expected QC.=== res

tests :: [TestTree]
tests =
  [ unit_zero {- overFlowInt -},
    unit_naive,
    unsafePerformIO spec_invalid,
    unit_sumMaybeBounds,
    QC.testProperty "maxRight" prop_maxRight,
    QC.testProperty "minLeft" prop_minLeft
  ]
