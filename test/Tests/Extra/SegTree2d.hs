{-# LANGUAGE RecordWildCards #-}

module Tests.Extra.SegTree2d (tests) where

import AtCoder.Extra.SegTree2d qualified as Seg
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST (RealWorld)
import Data.Foldable (for_)
import Data.Semigroup (Sum (..))
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import Test.QuickCheck.Monadic as QCM
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

data Init = Init
  { capacity :: {-# UNPACK #-} !Int,
    q :: {-# UNPACK #-} !Int,
    debugPoints :: VU.Vector (Int, Int),
    vecM :: !(IO (VUM.MVector RealWorld (Int, Int, Sum Int))),
    segM :: !(IO (Seg.SegTree2d RealWorld (Sum Int)))
  }

instance Show Init where
  show Init {..} = show ("Init", capacity, q, debugPoints)

instance QC.Arbitrary Init where
  arbitrary = do
    QC.Positive n <- QC.arbitrary
    q <- QC.chooseInt (1, 2 * n)
    yxs <- (VU.fromList <$>) $ QC.vectorOf n $ do
      x <- QC.chooseInt (-16, 16)
      y <- QC.chooseInt (-16, 16)
      pure (x, y)
    let vecM = VU.thaw $ VU.map (\(!x, !y) -> (x, y, mempty)) yxs
    pure $ Init n q yxs vecM (Seg.new yxs)

data Query
  = -- Read !Int |
    Write !Int !Int
  | ModifyAdd !Int !Int
  | Prod !(Int, Int, Int, Int)
  | AllProd
  | Count !(Int, Int, Int, Int)
  deriving
    ( Eq,
      Show
    )

genQuery :: Int -> QC.Gen Query
genQuery n = do
  QC.oneof
    [ -- Read <$> i,
      Write <$> i <*> val,
      ModifyAdd <$> val <*> i,
      Prod <$> rect,
      pure AllProd,
      Count <$> rect
    ]
  where
    i = QC.chooseInt (0, n - 1)
    rect = do
      xl <- QC.chooseInt (-18, 18)
      xr <- QC.chooseInt (xl, 18)
      yl <- QC.chooseInt (-18, 18)
      yr <- QC.chooseInt (yl, 18)
      pure (xl, xr, yl, yr)
    val = QC.arbitrary @Int

-- | Arbitrary return type for the `Query` result.
data Result
  = None
  | S !(Sum Int)
  | I !Int
  | MS !(Maybe (Sum Int))
  deriving (Show, Eq)

-- | containers. (referencial implementation)
handleRef :: VUM.MVector RealWorld (Int, Int, Sum Int) -> Query -> IO Result
handleRef vec q = case q of
  -- Read i -> do
  --   S . (\(!_, !_, !w) -> w) <$> VGM.read vec i
  Write i v -> do
    (!x, !y, !_) <- VGM.read vec i
    VGM.write vec i (x, y, Sum v)
    pure None
  ModifyAdd w i -> do
    VGM.modify vec (\(!x, !y, !w0) -> (x, y, w0 + Sum w)) i
    pure None
  Prod (!x1, !x2, !y1, !y2) -> do
    vec' <- VU.unsafeFreeze vec
    pure
      . S
      . VU.sum
      . VU.map (\(!_, !_, !w) -> w)
      $ VU.filter (\(!x, !y, !_) -> x1 <= x && x < x2 && y1 <= y && y < y2) vec'
  AllProd -> do
    vec' <- VU.unsafeFreeze vec
    pure . S . VU.sum $ VU.map (\(!_, !_, !w) -> w) vec'
  Count (!x1, !x2, !y1, !y2) -> do
    vec' <- VU.unsafeFreeze vec
    pure
      . I
      . VU.length
      $ VU.filter (\(!x, !y, !_) -> x1 <= x && x < x2 && y1 <= y && y < y2) vec'

handleAcl :: (PrimMonad m) => Seg.SegTree2d (PrimState m) (Sum Int) -> Query -> m Result
handleAcl seg q = case q of
  -- Read i -> do
  --   S <$> Seg.read seg i
  Write i v -> do
    Seg.write seg i $ Sum v
    pure None
  ModifyAdd w i -> do
    Seg.modify seg (+ Sum w) i
    pure None
  Prod (!x1, !x2, !y1, !y2) -> do
    S <$> Seg.prod seg x1 x2 y1 y2
  AllProd -> do
    S <$> Seg.allProd seg
  Count (!x1, !x2, !y1, !y2) -> do
    I <$> Seg.count seg x1 x2 y1 y2

prop_randomTest :: Init -> QC.Property
prop_randomTest Init {..} = QCM.monadicIO $ do
  seg <- QCM.run segM
  vec <- QCM.run vecM
  qs <- QCM.pick $ QC.vectorOf q (genQuery capacity)
  for_ qs $ \query -> do
    expected <- QCM.run $ handleRef vec query
    actual <- QCM.run $ handleAcl seg query
    QCM.assertWith (expected == actual) $ show (query, expected, actual)

unit_zero :: TestTree
unit_zero = testCase "zero" $ do
  seg <- Seg.build @_ @(Sum Int) VU.empty VU.empty VU.empty
  (@?= mempty) =<< Seg.prod seg 0 10 0 10
  (@?= mempty) =<< Seg.allProd seg
  (@?= 0) =<< Seg.count seg 0 10 0 10

tests :: [TestTree]
tests =
  [ unit_zero,
    QC.testProperty "random test" prop_randomTest
  ]
