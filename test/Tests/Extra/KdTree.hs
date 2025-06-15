{-# LANGUAGE RecordWildCards #-}

module Tests.Extra.KdTree where

import AtCoder.Extra.KdTree qualified as Kt
import Data.Foldable (for_)
import Data.Vector.Algorithms.Intro qualified as VAI
import Data.Vector.Unboxed qualified as VU
import Test.QuickCheck.Monadic as QCM
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Tests.Util
import Test.Tasty.HUnit

data Init = Init
  { n :: {-# UNPACK #-} !Int,
    q :: {-# UNPACK #-} !Int,
    refVec :: !(VU.Vector (Int, Int)),
    kt :: !Kt.KdTree
  }

rng :: (Int, Int)
rng = (-rngI, rngI)

rngI :: Int
rngI = 10

instance Show Init where
  show Init {..} = show n

instance QC.Arbitrary Init where
  arbitrary = do
    -- n <- QC.chooseInt (1, 256)
    n <- QC.chooseInt (1, 8)
    -- q <- QC.chooseInt (1, 256)
    q <- QC.chooseInt (1, 32)
    refVec <- (VU.fromList <$>) $ QC.vectorOf n $ do
      x <- QC.chooseInt rng
      y <- QC.chooseInt rng
      pure (x, y)
    let kt = Kt.build2 refVec
    pure Init {..}

data Query
  = FindPointsIn ((Int, Int), (Int, Int))
  | FindNearestPoint (Int, Int)
  deriving (Show)

-- | Arbitrary return type for the `Query` result.
data Result
  = MI !(Maybe Int)
  | V !(VU.Vector Int)
  deriving (Show, Eq)

queryGen :: Int -> QC.Gen Query
queryGen _n = do
  QC.oneof
    [ FindPointsIn <$> r,
      FindNearestPoint <$> xy
    ]
  where
    r = (,) <$> intervalGen' (-rngI) rngI <*> intervalGen' (-rngI) rngI
    xy = intervalGen' (-rngI) rngI

-- | containers. (referencial implementation)
handleRef :: VU.Vector (Int, Int) -> Query -> Result
handleRef vec q = case q of
  FindPointsIn ((!x1, !x2), (!y1, !y2)) -> do
    V $ VU.findIndices (\(!x, !y) -> x1 <= x && x < x2 && y1 <= y && y < y2) vec
  FindNearestPoint (!x, !y)
    | VU.null vec -> MI Nothing
    | otherwise -> MI
      . Just . VU.minIndex
        $ VU.map (\(!x', !y') -> (x - x') * (x - x') + (y - y') * (y - y')) vec

-- | ACL
handleAcl :: Kt.KdTree -> Query -> Result
handleAcl kt q = case q of
  FindPointsIn ((!x1, !x2), (!y1, !y2)) ->
    V $ Kt.findPointsIn kt x1 x2 y1 y2 (Kt.nKt kt)
  FindNearestPoint (!x, !y) ->
    MI $ Kt.findNearestPoint kt x y

prop_randomTest :: Init -> QC.Property
prop_randomTest Init {..} = QCM.monadicIO $ do
  qs <- QCM.pick $ QC.vectorOf q (queryGen n)
  for_ qs $ \query -> do
    let expected = handleRef refVec query
    let res = handleAcl kt query
    QCM.assertWith (p query expected res) $ show (query, expected, res)
  where
    p (FindNearestPoint (!x, !y)) (MI (Just a)) (MI (Just b)) =
      let (!x1, !y1) = refVec VU.! a
          (!x2, !y2) = refVec VU.! b
          d1 = (x1 - x) * (x1 - x) + (y1 - y) * (y1 - y)
          d2 = (x2 - x) * (x2 - x) + (y2 - y) * (y2 - y)
       in a == b || d1 == d2
    p (FindPointsIn _) (V a) (V b) = VU.modify VAI.sort a == VU.modify VAI.sort b
    p _ a b = a == b

unit_zero :: TestTree
unit_zero = testCase "zero" $ do
  let kt = Kt.build VU.empty VU.empty
  (@?= VU.empty) $ Kt.findPointsIn kt 0 10 0 10 5
  (@?= Nothing) $ Kt.findNearestPoint kt 10 10
  pure ()

tests :: [TestTree]
tests =
  [ QC.testProperty "randomTest" prop_randomTest,
    unit_zero
  ]
