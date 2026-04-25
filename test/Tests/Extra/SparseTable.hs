{-# LANGUAGE RecordWildCards #-}

module Tests.Extra.SparseTable (tests) where

import AtCoder.Extra.SparseTable qualified as Tbl
import Data.Foldable (for_)
import Data.List qualified as L
import Data.Ord (Down (..), comparing)
import Data.Semigroup (Max (..))
import Data.Vector qualified as V
import Data.Vector.Algorithms.Intro qualified as VAI
import Data.Vector.Unboxed qualified as VU
import Test.QuickCheck.Monadic as QCM
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Tests.Util
import Prelude hiding (seq)

data Init = Init
  { n :: {-# UNPACK #-} !Int,
    lr :: !(Int, Int),
    xs :: !(VU.Vector (Max Int))
  }
  deriving (Show)

instance QC.Arbitrary Init where
  arbitrary = do
    n <- QC.chooseInt (1, 64)
    lr <- intervalGen n
    xs <- VU.fromList <$> QC.vectorOf n QC.arbitrary
    pure Init {..}

prop_randomTest :: Init -> QC.Property
prop_randomTest Init {..} = Tbl.prod tbl l r QC.=== f l r
  where
    (!l, !r) = lr
    tbl = Tbl.new xs
    f l r = VU.foldl' (<>) mempty . VU.take (r - l) $ VU.drop l xs

prop_maxRight :: Init -> QC.Gen QC.Property
prop_maxRight Init {..} = do
  m <- QC.arbitrary
  pure $ Tbl.maxRight tbl l (< m) QC.=== f l m
  where
    xs' = VU.modify VAI.sort xs
    tbl = Tbl.new xs'
    n = VU.length xs'
    (!l, !_) = lr
    f l m = (l +) . VU.length . VU.takeWhile (< m) $ VU.drop l xs'

prop_minLeft :: Init -> QC.Gen QC.Property
prop_minLeft Init {..} = do
  m <- QC.arbitrary
  pure $ Tbl.minLeft tbl r (< m) QC.=== f r m
  where
    xs' = VU.modify (VAI.sortBy (comparing Down)) xs
    tbl = Tbl.new xs'
    n = VU.length xs'
    (!_, !r) = lr
    f r m = (r -) . VU.length . VU.takeWhile (< m) . VU.drop (n - r) $ VU.reverse xs'

tests :: [TestTree]
tests =
  [ QC.testProperty "random test" prop_randomTest,
    QC.testProperty "maxRight" prop_maxRight,
    QC.testProperty "minLeft" prop_minLeft
  ]
