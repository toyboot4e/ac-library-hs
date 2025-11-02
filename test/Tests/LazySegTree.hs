{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}

module Tests.LazySegTree (tests) where

import AtCoder.Extra.Monoid (Affine1)
import AtCoder.LazySegTree qualified as LSeg
import Data.Foldable (for_)
import Data.Semigroup (Max (..), Sum (..))
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hspec

-- | AddMax. FIXME: Orphan instance.
instance LSeg.SegAct (Sum Int) (Max Int) where
  {-# INLINE segAct #-}
  segAct (Sum !dx) (Max !x) = Max $ dx + x

unit_zero :: TestTree
unit_zero = testCase "zero" $ do
  do
    s <- LSeg.new @_ @(Sum Int) @(Max Int) 0
    (@?= mempty) =<< LSeg.allProd s
    (@?= VU.empty) =<< LSeg.unsafeFreeze s
    (@?= VU.empty) =<< LSeg.freeze s
  do
    s <- LSeg.new @_ @(Sum Int) @(Max Int) 10
    (@?= mempty) =<< LSeg.allProd s

-- Extra test for freeze
unit_one :: TestTree
unit_one = testCase "one" $ do
  s <- LSeg.build @_ @(Sum Int) @(Max Int) $ VU.singleton (Max 10)
  (@?= Max 10) =<< LSeg.allProd s
  (@?= VU.singleton (Max 10)) =<< LSeg.unsafeFreeze s
  (@?= VU.singleton (Max 10)) =<< LSeg.freeze s

-- assign

spec_invalid :: IO TestTree
spec_invalid = testSpec "invalid" $ do
  it "throws error" $ LSeg.new @_ @(Sum Int) @(Max Int) (-1) `shouldThrow` anyException
  s <- runIO $ LSeg.new @_ @(Sum Int) @(Max Int) 10

  it "throws error" $ LSeg.read s (-1) `shouldThrow` anyException
  it "throws error" $ LSeg.read s 10 `shouldThrow` anyException

  it "throws error" $ LSeg.prod s (-1) (-1) `shouldThrow` anyException
  it "throws error" $ LSeg.prod s 3 2 `shouldThrow` anyException
  it "throws error" $ LSeg.prod s 0 11 `shouldThrow` anyException
  it "throws error" $ LSeg.prod s (-1) 11 `shouldThrow` anyException

  it "throws error" $ LSeg.minLeft s (-1) (const True) `shouldThrow` anyException
  it "throws error" $ LSeg.maxRight s 0 (const False) `shouldThrow` anyException

-- TODO: verify yosupo

unit_naiveProd :: TestTree
unit_naiveProd = testCase "naiveProd" $ do
  for_ [0 .. 50] $ \n -> do
    seg <- LSeg.new @_ @(Sum Int) @(Max Int) n
    p <- VUM.replicate n mempty
    for_ [0 .. n - 1] $ \i -> do
      VGM.write p i . Max $ (i * i + 100) `mod` 31
      LSeg.write seg i =<< VGM.read p i

    p' <- VU.unsafeFreeze p
    for_ [0 .. n] $ \l -> do
      for_ [l .. n] $ \r -> do
        let expected = VU.foldl' (<>) mempty $ VU.slice l (r - l) p'
        (@?= expected) =<< LSeg.prod seg l r

    if even n
      then do
        -- read (extra test)
        for_ [0 .. n - 1] $ \i -> do
          (@?= p' VG.! i) =<< LSeg.read seg i
      else do
        -- freeze (extra test)
        (@?= p') =<< LSeg.unsafeFreeze seg

unit_usage :: TestTree
unit_usage = testCase "usage" $ do
  seg <- LSeg.build @_ @(Sum Int) $ VU.replicate 10 (Max (0 :: Int))
  (@?= Max 0) =<< LSeg.allProd seg
  LSeg.applyIn seg 0 3 $ Sum 5
  (@?= Max 5) =<< LSeg.allProd seg
  LSeg.applyAt seg 2 $ Sum (-10)
  (@?= Max (-5)) =<< LSeg.prod seg 2 3
  (@?= Max 5) =<< LSeg.allProd seg
  (@?= Max 0) =<< LSeg.prod seg 2 4

unit_prodMaybeBounds :: TestTree
unit_prodMaybeBounds = testCase "prodMaybeBounds" $ do
  seg <- LSeg.new @_ @(Affine1 Int) @(Sum Int) 4
  (@?= Just (Sum {getSum = 0})) =<< LSeg.prodMaybe seg 0 0
  (@?= Just (Sum {getSum = 0})) =<< LSeg.prodMaybe seg 0 4
  (@?= Just (Sum {getSum = 0})) =<< LSeg.prodMaybe seg 4 4
  (@?= Nothing) =<< LSeg.prodMaybe seg (-1) 4
  (@?= Nothing) =<< LSeg.prodMaybe seg 0 5
  (@?= Nothing) =<< LSeg.prodMaybe seg 0 (-1)
  (@?= Nothing) =<< LSeg.prodMaybe seg (-1) (-1)
  (@?= Nothing) =<< LSeg.prodMaybe seg (-1) 0
  (@?= Nothing) =<< LSeg.prodMaybe seg 4 5
  (@?= Nothing) =<< LSeg.prodMaybe seg 5 5

-- maxRight and minLeft are tested in the stress test file.

tests :: [TestTree]
tests =
  [ unit_zero,
    unit_one,
    unsafePerformIO spec_invalid,
    unit_naiveProd,
    unit_usage,
    unit_prodMaybeBounds
  ]
