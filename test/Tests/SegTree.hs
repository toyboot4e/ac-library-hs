{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Tests.SegTree (tests) where

import Data.Monoid (Sum(..))
import AtCoder.Internal.Assert
import AtCoder.SegTree qualified as ST
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Char (chr, ord)
import Data.Foldable
import Data.Monoid
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hspec
import Util (DoNotUnboxLazy (..))

data SegTreeNaive s a = SegTreeNaive
  { nStn :: {-# UNPACK #-} !Int,
    dStn :: !(VUM.MVector s a)
  }

newStn :: (PrimMonad m, Monoid a, VU.Unbox a) => Int -> m (SegTreeNaive (PrimState m) a)
newStn nStn = do
  dStn <- VUM.replicate nStn mempty
  pure SegTreeNaive {..}

writeStn :: (VU.Unbox a, PrimMonad m) => SegTreeNaive (PrimState m) a -> Int -> a -> m ()
writeStn SegTreeNaive {..} = VGM.write dStn

readStn :: (VU.Unbox a, PrimMonad m) => SegTreeNaive (PrimState m) a -> Int -> m a
readStn SegTreeNaive {..} = VGM.read dStn

prodStn :: (Monoid a, VU.Unbox a, PrimMonad m) => SegTreeNaive (PrimState m) a -> Int -> Int -> m a
prodStn SegTreeNaive {..} l r = do
  d <- VU.unsafeFreeze dStn
  let slice = VU.take (r - l) $ VU.drop l d
  pure $ VU.foldl' (<>) mempty slice

maxRightStn :: (HasCallStack, Monoid a, VU.Unbox a, PrimMonad m) => SegTreeNaive (PrimState m) a -> Int -> (a -> Bool) -> m Int
maxRightStn SegTreeNaive {..} l0 f = do
  let !_ = runtimeAssert (f mempty) "ident"
  let loop _ _ [] = nStn
      loop l !acc (x : xs)
        | not (f acc') = l
        | otherwise = loop (l + 1) acc' xs
        where
          !acc' = acc <> x
  d <- VU.unsafeFreeze dStn
  pure . loop l0 mempty . VU.toList $ VU.drop l0 d

minLeftStn :: (HasCallStack, Monoid a, VU.Unbox a, PrimMonad m) => SegTreeNaive (PrimState m) a -> Int -> (a -> Bool) -> m Int
minLeftStn SegTreeNaive {..} r0 f = do
  let !_ = runtimeAssert (f mempty) "ident"
  let loop _ _ [] = 0
      loop r !acc (x : xs)
        | not (f acc') = r + 1
        | otherwise = loop (r - 1) acc' xs
        where
          !acc' = x <> acc
  d <- VU.unsafeFreeze dStn
  pure . loop (r0 - 1) mempty . VU.toList . VU.reverse $ VU.take r0 d

freezeStn :: (Monoid a, VU.Unbox a, PrimMonad m) => SegTreeNaive (PrimState m) a -> m (VU.Vector a)
freezeStn = VU.freeze . dStn

type FooRepr = DoNotUnboxLazy String

newtype Foo = Foo String
  deriving (Eq, Ord, Show)

instance Semigroup Foo where
  (<>) :: (HasCallStack) => Foo -> Foo -> Foo
  (Foo a) <> (Foo b) = Foo $ case (a, b) of
    ("$", _) -> b
    (_, "$") -> a
    _ -> a <> b
    where
      !_ = runtimeAssert (a == "$" || b == "$" || a <= b) "error"

instance Monoid Foo where
  mempty = Foo "$"

instance VU.IsoUnbox Foo FooRepr where
  {-# INLINE toURepr #-}
  toURepr (Foo s) = DoNotUnboxLazy s
  {-# INLINE fromURepr #-}
  fromURepr (DoNotUnboxLazy s) = Foo s

newtype instance VU.MVector s Foo = MV_Foo (VU.MVector s FooRepr)

newtype instance VU.Vector Foo = V_Foo (VU.Vector FooRepr)

deriving via (Foo `VU.As` FooRepr) instance VGM.MVector VUM.MVector Foo

deriving via (Foo `VU.As` FooRepr) instance VG.Vector VU.Vector Foo

instance VU.Unbox Foo

unit_zero :: TestTree
unit_zero = testCase "zero" $ do
  s <- ST.new @_ @(Sum Int) 0
  (@?= mempty) =<< ST.allProd s

  pure ()

spec_invalid :: IO TestTree
spec_invalid = testSpec "invalid" $ do
  it "throws error" $
    ST.new @_ @(Sum Int) (-1) `shouldThrow` anyException
  s <- runIO $ ST.new @_ @(Sum Int) 10

  it "throws error" $ ST.read s (-1) `shouldThrow` anyException
  it "throws error" $ ST.read s 10 `shouldThrow` anyException

  it "throws error" $ ST.write s (-1) 0 `shouldThrow` anyException
  it "throws error" $ ST.write s 10 0 `shouldThrow` anyException
  it "throws error" $ ST.modify s (+ 1) (-1) `shouldThrow` anyException
  it "throws error" $ ST.modify s (+ 1) 10 `shouldThrow` anyException
  it "throws error" $ ST.modifyM s (pure . (+ 1)) (-1) `shouldThrow` anyException
  it "throws error" $ ST.modifyM s (pure . (+ 1)) 10 `shouldThrow` anyException

  it "throws error" $ ST.prod s (-1) (-1) `shouldThrow` anyException
  it "throws error" $ ST.prod s 3 2 `shouldThrow` anyException
  it "throws error" $ ST.prod s 0 11 `shouldThrow` anyException
  it "throws error" $ ST.prod s (-1) 11 `shouldThrow` anyException

  it "throws error" $ ST.maxRight s 11 (const True) `shouldThrow` anyException
  it "throws error" $ ST.minLeft s (-1) (const True) `shouldThrow` anyException
  it "throws error" $ ST.maxRight s 0 (const False) `shouldThrow` anyException

unit_one :: TestTree
unit_one = testCase "one" $ do
  seg <- ST.new @_ @(Sum Int) 1
  (@?= mempty) =<< ST.allProd seg
  (@?= mempty) =<< ST.read seg 0
  (@?= mempty) =<< ST.prod seg 0 1
  let dummy = Sum 42
  ST.write seg 0 dummy
  (@?= dummy) =<< ST.read seg 0
  (@?= mempty) =<< ST.prod seg 0 0
  (@?= dummy) =<< ST.prod seg 0 1
  (@?= mempty) =<< ST.prod seg 1 1

unit_compareNaive :: TestTree
unit_compareNaive = testCase "compareNaive" $ do
  run $ \seg i x -> ST.write seg i x
  run $ \seg i x -> ST.modify seg (const x) i
  run $ \seg i x -> ST.modifyM seg (pure . (const x)) i
  where
    run f = do
      for_ [0 .. 30 - 1] $ \n -> do
        seg0 <- newStn @_ @Foo n
        seg1 <- ST.new @_ @Foo n
        -- write
        for_ [0 .. n - 1] $ \i -> do
          writeStn seg0 i . Foo . (: []) . chr $ ord 'a' + i
          (f seg1 i) . Foo . (: []) . chr $ ord 'a' + i

        -- prod
        for_ [0 .. n] $ \l -> do
          for_ [l .. n] $ \r -> do
            x0 <- prodStn seg0 l r
            x1 <- ST.prod seg1 l r
            assertEqual (show (l, r)) x0 x1

        -- allProd
        do
          x1 <- prodStn seg0 0 n
          x2 <- ST.prod seg1 0 n
          x3 <- ST.allProd seg1
          x1 @?= x2
          x1 @?= x3

        -- maxRight
        for_ [0 .. n] $ \l -> do
          for_ [l .. n] $ \r -> do
            Foo y <- ST.prod seg1 l r
            let p :: Foo -> Bool
                p (Foo x) = length x <= length y
            r0 <- maxRightStn seg0 l p
            r1 <- ST.maxRight seg1 l p
            assertEqual (show ((l, r), y)) r0 r1

        -- minLeft
        for_ [0 .. n] $ \r -> do
          for_ [0 .. r] $ \l -> do
            Foo y <- ST.prod seg1 l r
            let p :: Foo -> Bool
                p (Foo x) = length x <= length y
            r0 <- minLeftStn seg0 r p
            r1 <- ST.minLeft seg1 r p
            assertEqual (show ((l, r), y)) r0 r1

        -- read (extra test)
        for_ [0 .. n - 1] $ \i -> do
          expected <- readStn seg0 i
          (@?= expected) =<< ST.read seg1 i

        -- freeze (extra test)
        expected <- freezeStn seg0
        (@?= expected) =<< ST.freeze seg1

unit_freezeZero :: TestTree
unit_freezeZero = testCase "freezeZero" $ do
  seg <- ST.new @_ @(Sum Int) 0
  (VU.empty @=?) =<< ST.freeze seg
  (VU.empty @=?) =<< ST.unsafeFreeze seg

unit_prodMaybeBounds :: TestTree
unit_prodMaybeBounds = testCase "prodMaybeBounds" $ do
  seg <- ST.new @_ @(Sum Int) 4
  (@?= Just (Sum {getSum = 0})) =<< ST.prodMaybe seg 0 0
  (@?= Just (Sum {getSum = 0})) =<< ST.prodMaybe seg 0 4
  (@?= Just (Sum {getSum = 0})) =<< ST.prodMaybe seg 4 4
  (@?= Nothing) =<< ST.prodMaybe seg (-1) 4
  (@?= Nothing) =<< ST.prodMaybe seg 0 5
  (@?= Nothing) =<< ST.prodMaybe seg 0 (-1)
  (@?= Nothing) =<< ST.prodMaybe seg (-1) (-1)
  (@?= Nothing) =<< ST.prodMaybe seg (-1) 0
  (@?= Nothing) =<< ST.prodMaybe seg 4 5
  (@?= Nothing) =<< ST.prodMaybe seg 5 5

tests :: [TestTree]
tests =
  [ unit_zero,
    unsafePerformIO spec_invalid,
    unit_one,
    unit_compareNaive,
    unit_freezeZero,
    unit_prodMaybeBounds
  ]
