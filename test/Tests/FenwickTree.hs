-- | Fenwick tree tests.
module Tests.FenwickTree (tests) where

import AtCoder.FenwickTree qualified as FT
import Data.Foldable
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hspec

-- empty
-- assign

zero :: TestTree
zero = testCase "zero" $ do
  fw <- FT.new @Int 0
  (@?= 0) =<< FT.sum fw 0 0

-- TODO: add modint test

-- FIXME: it's uint test and not works with Int
-- overFlowInt :: TestTree
-- overFlowInt = testCase "overFlowInt" $ do
--   fw <- FT.new @Int 10
--   for_ [0 .. 10 - 1] $ \i -> do
--     FT.add fw i (bit 63 + i)
--   for_ [0 .. 10] $ \i -> do
--     for_ [0 .. 10] $ \j -> do
--       let s = sum [i .. j - 1]
--       let expected = if (j - i) `mod` 2 == 1 then bit 63 + s else s
--       result <- FT.sum fw i j
--       result @?= expected

naive :: TestTree
naive = testCase "naive" $ do
  for_ [0 .. 50] $ \n -> do
    fw <- FT.new @Int n
    for_ [0 .. n - 1] $ \i -> do
      FT.add fw i (i * i)
    for_ [0 .. n] $ \l -> do
      for_ [l .. n] $ \r -> do
        let s = sum [i * i | i <- [l .. r - 1]]
        (@?= s) =<< FT.sum fw l r

-- TODO: smint
-- TODO: mint

invalid :: IO TestTree
invalid = testSpec "invalid" $ do
  it "throws error" $ do
    FT.new @Int (-1) `shouldThrow` anyException
  s <- runIO $ FT.new @Int 10

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

tests :: [TestTree]
tests =
  [zero {- overFlowInt -}, naive, unsafePerformIO invalid]
