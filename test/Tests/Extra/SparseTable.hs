{-# LANGUAGE RecordWildCards #-}

module Tests.Extra.SparseTable (tests) where

import Test.QuickCheck.Monadic as QCM
import AtCoder.Extra.SparseTable qualified as Tbl
import Data.Foldable (for_
                     )
import Data.List qualified as L
import Data.Semigroup (Max (..))
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
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

tests :: [TestTree]
tests =
  [ QC.testProperty "random test" prop_randomTest
  ]
