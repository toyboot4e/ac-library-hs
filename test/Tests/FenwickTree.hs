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

tests :: [TestTree]
tests =
  [unit_zero {- overFlowInt -}, unit_naive, unsafePerformIO spec_invalid]
