{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}

module Tests.LazySegTree (tests) where

import AtCoder.Extra.Monoid (Affine1)
import AtCoder.LazySegTree qualified as LST
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
instance LST.SegAct (Sum Int) (Max Int) where
  {-# INLINE segAct #-}
  segAct (Sum !dx) (Max !x) = Max $ dx + x

unit_zero :: TestTree
unit_zero = testCase "zero" $ do
  do
    s <- LST.new @_ @(Sum Int) @(Max Int) 0
    (@?= mempty) =<< LST.allProd s
    (@?= VU.empty) =<< LST.unsafeFreeze s
    (@?= VU.empty) =<< LST.freeze s
  do
    s <- LST.new @_ @(Sum Int) @(Max Int) 10
    (@?= mempty) =<< LST.allProd s

-- Extra test for freeze
unit_one :: TestTree
unit_one = testCase "one" $ do
  s <- LST.build @_ @(Sum Int) @(Max Int) $ VU.singleton (Max 10)
  (@?= Max 10) =<< LST.allProd s
  (@?= VU.singleton (Max 10)) =<< LST.unsafeFreeze s
  (@?= VU.singleton (Max 10)) =<< LST.freeze s

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

  it "throws error" $ LST.minLeft s (-1) (const True) `shouldThrow` anyException
  it "throws error" $ LST.maxRight s 0 (const False) `shouldThrow` anyException

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

    if even n
      then do
        -- read (extra test)
        for_ [0 .. n - 1] $ \i -> do
          (@?= p' VG.! i) =<< LST.read seg i
      else do
        -- freeze (extra test)
        (@?= p') =<< LST.unsafeFreeze seg

unit_usage :: TestTree
unit_usage = testCase "usage" $ do
  seg <- LST.build @_ @(Sum Int) $ VU.replicate 10 (Max (0 :: Int))
  (@?= Max 0) =<< LST.allProd seg
  LST.applyIn seg 0 3 $ Sum 5
  (@?= Max 5) =<< LST.allProd seg
  LST.applyAt seg 2 $ Sum (-10)
  (@?= Max (-5)) =<< LST.prod seg 2 3
  (@?= Max 0) =<< LST.prod seg 2 4

unit_prodMaybeBounds :: TestTree
unit_prodMaybeBounds = testCase "prodMaybeBounds" $ do
  seg <- LST.new @_ @(Affine1 Int) @(Sum Int) 4
  (@?= Just (Sum {getSum = 0})) =<< LST.prodMaybe seg 0 0
  (@?= Just (Sum {getSum = 0})) =<< LST.prodMaybe seg 0 4
  (@?= Just (Sum {getSum = 0})) =<< LST.prodMaybe seg 4 4
  (@?= Nothing) =<< LST.prodMaybe seg (-1) 4
  (@?= Nothing) =<< LST.prodMaybe seg 0 5
  (@?= Nothing) =<< LST.prodMaybe seg 0 (-1)
  (@?= Nothing) =<< LST.prodMaybe seg (-1) (-1)
  (@?= Nothing) =<< LST.prodMaybe seg (-1) 0
  (@?= Nothing) =<< LST.prodMaybe seg 4 5
  (@?= Nothing) =<< LST.prodMaybe seg 5 5

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
