{-# LANGUAGE RecordWildCards #-}

module Tests.Extra.SegTree2d.Dense (tests) where

import AtCoder.Extra.SegTree2d.Dense qualified as Seg
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
  { wQ :: {-# UNPACK #-} !Int,
    hQ :: {-# UNPACK #-} !Int,
    q :: {-# UNPACK #-} !Int,
    vecM :: !(IO (VUM.MVector RealWorld (Sum Int))),
    segM :: !(IO (Seg.DenseSegTree2d RealWorld (Sum Int)))
  }

instance Show Init where
  show Init {..} = show ("Init", wQ, hQ, q)

instance QC.Arbitrary Init where
  arbitrary = do
    QC.Positive q <- QC.arbitrary
    w <- QC.chooseInt (1, 15)
    h <- QC.chooseInt (1, 15)
    let vecM = VUM.replicate (w * h) mempty
    pure $ Init w h q vecM (Seg.new w h)

data Query
  = Read !(Int, Int)
  | ReadMaybe !(Int, Int)
  | Write !(Int, Int) !Int
  | ModifyAdd !Int !(Int, Int)
  | Prod !(Int, Int, Int, Int)
  | AllProd
  deriving
    ( Eq,
      Show
    )

genQuery :: Int -> Int -> QC.Gen Query
genQuery w h = do
  QC.oneof
    [ Read <$> xy,
      ReadMaybe <$> xy',
      Write <$> xy <*> val,
      ModifyAdd <$> val <*> xy,
      Prod <$> rect,
      pure AllProd
    ]
  where
    xy = (,) <$> QC.chooseInt (0, w - 1) <*> QC.chooseInt (0, h - 1)
    xy' = (,) <$> QC.chooseInt (-1, w) <*> QC.chooseInt (-1, h)
    rect = do
      xl <- QC.chooseInt (-1, w)
      xr <- QC.chooseInt (xl, w)
      yl <- QC.chooseInt (-1, h)
      yr <- QC.chooseInt (yl, h)
      pure (xl, xr, yl, yr)
    val = QC.arbitrary @Int

-- | Arbitrary return type for the `Query` result.
data Result
  = None
  | S !(Sum Int)
  | I !Int
  | MS !(Maybe (Sum Int))
  deriving (Show, Eq)

-- | containers. (referential implementation)
handleRef :: Int -> Int -> VUM.MVector RealWorld (Sum Int) -> Query -> IO Result
handleRef w h vec q = case q of
  Read (!x, !y) -> do
    S <$> VGM.read vec (w * y + x)
  ReadMaybe (!x, !y)
    | 0 <= x && x < w && 0 <= y && y < h -> MS . Just <$> VGM.read vec (w * y + x)
    | otherwise -> pure $ MS Nothing
  Write (!x, !y) v -> do
    VGM.write vec (w * y + x) (Sum v)
    pure None
  ModifyAdd dw (!x, !y) -> do
    -- FIXME: why die?
    VGM.modify vec (+ Sum dw) (w * y + x)
    pure None
  Prod (!x1, !x2, !y1, !y2) -> do
    vec' <- VU.unsafeFreeze vec
    pure
      . S
      . VU.sum
      $ VU.ifilter
        ( \i _ ->
            let (!y, !x) = i `divMod` w
             in x1 <= x && x < x2 && y1 <= y && y < y2
        )
        vec'
  AllProd -> do
    vec' <- VU.unsafeFreeze vec
    pure . S $ VU.sum vec'

handleAcl :: (PrimMonad m) => Seg.DenseSegTree2d (PrimState m) (Sum Int) -> Query -> m Result
handleAcl seg q = case q of
  Read (!x, !y) -> do
    S <$> Seg.read seg x y
  ReadMaybe (!x, !y) -> do
    MS <$> Seg.readMaybe seg x y
  Write (!x, !y) v -> do
    Seg.write seg x y $ Sum v
    pure None
  ModifyAdd w (!x, !y) -> do
    Seg.modify seg (+ Sum w) x y
    pure None
  Prod (!x1, !x2, !y1, !y2) -> do
    S <$> Seg.prod seg x1 x2 y1 y2
  AllProd -> do
    S <$> Seg.allProd seg

prop_randomTest :: Init -> QC.Property
prop_randomTest Init {..} = QCM.monadicIO $ do
  seg <- QCM.run segM
  vec <- QCM.run vecM
  qs <- QCM.pick $ QC.vectorOf q (genQuery wQ hQ)
  for_ qs $ \query -> do
    expected <- QCM.run $ handleRef wQ hQ vec query
    actual <- QCM.run $ handleAcl seg query
    QCM.assertWith (expected == actual) $ show (query, expected, actual)

unit_zero :: TestTree
unit_zero = testCase "zero" $ do
  seg <- Seg.new @_ @(Sum Int) 0 0
  (@?= mempty) =<< Seg.prod seg 0 10 0 10
  (@?= mempty) =<< Seg.allProd seg
  pure ()

tests :: [TestTree]
tests =
  [ unit_zero,
    QC.testProperty "random test" prop_randomTest
  ]
