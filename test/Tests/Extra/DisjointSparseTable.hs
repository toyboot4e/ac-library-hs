{-# LANGUAGE RecordWildCards #-}

module Tests.Extra.DisjointSparseTable (tests) where

import AtCoder.Extra.DisjointSparseTable qualified as Tbl
import Data.Semigroup (Sum (..))
import Data.Vector.Unboxed qualified as VU
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck as QC
import Tests.Util (intervalGen)
import Prelude hiding (seq)

data Init = Init
  { n :: {-# UNPACK #-} !Int,
    lr :: !(Int, Int),
    xs :: !(VU.Vector (Sum Int))
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
    tbl = Tbl.build xs
    f l r = VU.foldl' (<>) mempty . VU.take (r - l) $ VU.drop l xs

tests :: [TestTree]
tests =
  [ QC.testProperty "random test" prop_randomTest
  ]
