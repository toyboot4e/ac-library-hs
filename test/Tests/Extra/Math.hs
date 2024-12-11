module Tests.Extra.Math (tests) where

import AtCoder.Extra.Math qualified as ACEM
import Data.Proxy (Proxy (..))
import Data.Semigroup (Max (..), Min (..), Sum (..), mtimesDefault, stimes)
import Test.QuickCheck.Property qualified as QC
import Test.Tasty
import Test.Tasty.QuickCheck qualified as QC
import Tests.Util (myForAllShrink)

-- orphan instance
instance QC.Arbitrary (Max Int) where
  arbitrary = Max <$> QC.arbitrary

-- orphan instance
instance QC.Arbitrary (Min Int) where
  arbitrary = Min <$> QC.arbitrary

prop_stimes' :: forall a. (Semigroup a, Show a, Eq a, QC.Arbitrary a) => Proxy a -> QC.Property
prop_stimes' _ = myForAllShrink True (const True) desc lhsS lhs rhsS rhs
  where
    desc :: (QC.Positive Int, a) -> [String]
    desc (QC.Positive !n, !s) = ["n = " ++ show n ++ ", s = " ++ show s]
    lhsS = "stimes' n s"
    lhs (QC.Positive !n, !s) = ACEM.stimes' n s
    rhsS = "stimes n s"
    rhs (QC.Positive !n, !s) = stimes n s

prop_mtimes' :: forall a. (Monoid a, Show a, Eq a, QC.Arbitrary a) => Proxy a -> QC.Property
prop_mtimes' _ = myForAllShrink True (const True) desc lhsS lhs rhsS rhs
  where
    desc :: (QC.NonNegative Int, a) -> [String]
    desc (QC.NonNegative !n, !m) = ["n = " ++ show n ++ ", m = " ++ show m]
    lhsS = "mtimes' n s"
    lhs (QC.NonNegative !n, !m) = ACEM.mtimes' n m
    rhsS = "mtimes n s"
    rhs (QC.NonNegative !n, !m) = mtimesDefault n m

tests :: [TestTree]
tests =
  [ testGroup
      "stimes'"
      [ QC.testProperty "Sum" (prop_stimes' (Proxy @(Sum Int))),
        QC.testProperty "Product" (prop_stimes' (Proxy @(Sum Int))),
        QC.testProperty "Max" (prop_stimes' (Proxy @(Max Int))),
        QC.testProperty "Min" (prop_stimes' (Proxy @(Min Int)))
      ],
    testGroup
      "mtimes'"
      [ QC.testProperty "Sum" (prop_mtimes' (Proxy @(Sum Int))),
        QC.testProperty "Product" (prop_mtimes' (Proxy @(Sum Int))),
        QC.testProperty "Max" (prop_mtimes' (Proxy @(Max Int))),
        QC.testProperty "Min" (prop_mtimes' (Proxy @(Min Int)))
      ]
  ]
