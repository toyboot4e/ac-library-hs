{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Tests.LazySegTree (tests) where

import AtCoder.LazySegTree qualified as LST
import Data.Foldable
import Data.Monoid
import Data.Semigroup
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hspec

-- | AddMax. Orphan instance is.. ok.
instance LST.SegAct (Sum Int) (Max Int) where
  {-# INLINE segAct #-}
  segAct (Sum !dx) (Max !x) = Max $ dx + x

unit_zero :: TestTree
unit_zero = testCase "zero" $ do
  do
    s <- LST.new @_ @(Sum Int) @(Max Int) 0
    (@?= mempty) =<< LST.allProd s
  do
    s <- LST.new @_ @(Sum Int) @(Max Int) 10
    (@?= mempty) =<< LST.allProd s

-- assign

spec_invalid :: IO TestTree
spec_invalid = testSpec "invalid" $ do
  it "throws error" $ LST.new @_ @(Sum Int) @(Max Int) (-1) `shouldThrow` anyException
  s <- runIO $ LST.new @_ @(Sum Int) @(Max Int) 10

  it "throws error" $ LST.read s (-1) `shouldThrow` anyException
  it "throws error" $ LST.read s 10 `shouldThrow` anyException

  it "throws error" $ LST.prod s (-1) (-1) `shouldThrow` anyException
  it "throws error" $ LST.prod s 3 2 `shouldThrow` anyException
  it "throws error" $ LST.prod s 0 11 `shouldThrow` anyException
  it "throws error" $ LST.prod s (-1) 11 `shouldThrow` anyException

-- TODO: verify yosupo

unit_naiveProd :: TestTree
unit_naiveProd = testCase "naiveProd" $ do
  for_ [0 .. 50] $ \n -> do
    seg <- LST.new @_ @(Sum Int) @(Max Int) n
    p <- VUM.replicate n mempty
    for_ [0 .. n - 1] $ \i -> do
      VGM.write p i . Max $ (i * i + 100) `mod` 31
      LST.write seg i =<< VGM.read p i

    p' <- VU.unsafeFreeze p
    for_ [0 .. n] $ \l -> do
      for_ [l .. n] $ \r -> do
        let expected = VU.foldl' (<>) mempty $ VU.slice l (r - l) p'
        (@?= expected) =<< LST.prod seg l r

    -- read (extra test)
    for_ [0 .. n - 1] $ \i -> do
      (@?= p' VG.! i) =<< LST.read seg i

unit_usage :: TestTree
unit_usage = testCase "usage" $ do
  seg <- LST.build @_ @(Sum Int) $ VU.replicate 10 (Max (0 :: Int))
  (@?= Max 0) =<< LST.allProd seg
  LST.applyIn seg 0 3 $ Sum 5
  (@?= Max 5) =<< LST.allProd seg
  LST.applyAt seg 2 $ Sum (-10)
  (@?= Max (-5)) =<< LST.prod seg 2 3
  (@?= Max 0) =<< LST.prod seg 2 4

-- TODO: maxRight, minLeft tests

tests :: [TestTree]
tests =
  [unit_zero, unsafePerformIO spec_invalid, unit_naiveProd, unit_usage]
