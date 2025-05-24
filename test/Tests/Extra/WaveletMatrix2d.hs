{-# LANGUAGE RecordWildCards #-}

module Tests.Extra.WaveletMatrix2d (tests) where

import AtCoder.Extra.WaveletMatrix2d qualified as WM
import Control.Monad (foldM_)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST (RealWorld)
import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Semigroup (Sum (..))
import Data.Vector.Unboxed qualified as VU
import Test.QuickCheck.Monadic as QCM
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Tests.Util (intervalGen)

data Init = Init
  { capacity :: {-# UNPACK #-} !Int,
    map0 :: !(M.Map (Int, Int) (Sum Int)),
    wmM :: !(IO (WM.WaveletMatrix2d RealWorld (Sum Int)))
  }

instance Show Init where
  show Init {..} = show ("Init", capacity, map0)

instance QC.Arbitrary Init where
  arbitrary = do
    QC.NonNegative n <- QC.arbitrary
    let yxs = VU.fromList [(x, y) | x <- [-16 .. 16], y <- [-16 .. 16]]
    pure $ Init n M.empty (WM.new negate yxs)

data Query
  = Read !(Int, Int)
  | Write !(Int, Int) !Int
  | ModifyAdd !Int !(Int, Int)
  | Prod !(Int, Int) !(Int, Int)
  | ProdMaybe !(Int, Int) !(Int, Int)
  | AllProd
  deriving (Show)

genQuery :: Int -> QC.Gen Query
genQuery n = do
  QC.oneof
    [ Read <$> lr,
      Write <$> lr <*> val,
      ModifyAdd <$> val <*> lr,
      Prod <$> lr <*> lr,
      ProdMaybe <$> lr <*> lr,
      pure AllProd
    ]
  where
    lr = (\(!x, !y) -> (x - 16, y - 16)) <$> intervalGen 32
    val = QC.arbitrary @Int

-- | Arbitrary return type for the `Query` result.
data Result
  = None
  | S !(Sum Int)
  | MS !(Maybe (Sum Int))
  deriving (Show, Eq)

-- | containers. (referencial implementation)
handleRef :: M.Map (Int, Int) (Sum Int) -> Query -> (Result, M.Map (Int, Int) (Sum Int))
handleRef map q = case q of
  Read (!x, !y) -> (S . fromMaybe mempty $ M.lookup (x, y) map, map)
  Write (!x, !y) v -> (None, M.insert (x, y) (Sum v) map)
  ModifyAdd w (!x, !y) -> (None, M.insertWith (+) (x, y) (Sum w) map)
  Prod (!x1, !x2) (!y1, !y2) -> (S $ prod x1 x2 y1 y2, map)
  ProdMaybe (!x1, !x2) (!y1, !y2) -> (MS . Just $ prod x1 x2 y1 y2, map)
  AllProd -> (S $ L.foldl' (<>) mempty (M.elems map), map)
  where
    prod x1 x2 y1 y2 =
      L.foldl' (<>) (mempty :: Sum Int)
        . (snd <$>)
        . filter (\((!x, !y), !_) -> x1 <= x && x < x2 && y1 <= y && y < y2)
        $ M.assocs map

handleAcl :: (PrimMonad m) => WM.WaveletMatrix2d (PrimState m) (Sum Int) -> Query -> m Result
handleAcl wm q = case q of
  Read (!x, !y) -> do
    S <$> WM.read wm (x, y)
  Write (!x, !y) v -> do
    WM.write wm (x, y) $ Sum v
    pure None
  ModifyAdd w (!x, !y) -> do
    WM.modify wm (+ Sum w) (x, y)
    pure None
  Prod (!x1, !x2) (!y1, !y2) -> do
    S <$> WM.prod wm x1 x2 y1 y2
  ProdMaybe (!x1, !x2) (!y1, !y2) -> do
    MS <$> WM.prodMaybe wm x1 x2 y1 y2
  AllProd -> do
    S <$> WM.allProd wm

prop_randomTest :: Init -> QC.Property
prop_randomTest Init {..} = QCM.monadicIO $ do
  wm <- QCM.run wmM
  qs <- QCM.pick $ QC.vectorOf capacity (genQuery capacity)
  foldM_
    ( \map query -> do
        let (!expected, !map') = handleRef map query
        actual <- QCM.run $ handleAcl wm query
        QCM.assertWith (expected == actual) $ show (query, expected, actual)
        pure map'
    )
    map0
    qs

tests :: [TestTree]
tests =
  [ -- unit_boundary,
    QC.testProperty "random test" prop_randomTest
  ]
