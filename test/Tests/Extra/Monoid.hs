module Tests.Extra.Monoid (tests) where

import AtCoder.Extra.Monoid
import AtCoder.Extra.Monoid.Affine1 qualified as A
import Data.Bit (Bit (..))
import Data.Proxy (Proxy (..))
import Data.Semigroup (Max (..), Min (..), Product (..), Sum (..), stimes)
import Test.QuickCheck.Classes qualified as QCC
import Test.QuickCheck.Property qualified as QC
import Test.Tasty
import Test.Tasty.QuickCheck qualified as QC
import Tests.Util (laws, myForAllShrink)

-- TODO: (const True) should be removed

segActLaw :: (Monoid a, Eq a) => (SegAct f a, QC.Arbitrary f, Eq f, Show f, QC.Arbitrary f, QC.Arbitrary a, Show a) => Proxy (f, a) -> QCC.Laws
segActLaw p =
  QCC.Laws
    "SegAct"
    [ ("Identity map", segActIdentity p),
      ("Monoid Action", segActMonoidAction p),
      ("Linear Monoid Action", segActLinearMonoidAction p),
      ("Endomorphism", segActEndomorphism p)
    ]

segActIdentity :: forall f a. (SegAct f a, QC.Arbitrary f, Eq f, Show f, Monoid a, Eq a, QC.Arbitrary a, Show a) => Proxy (f, a) -> QC.Property
segActIdentity _ = myForAllShrink True (const True) desc lhsS lhs rhsS rhs
  where
    desc :: a -> [String]
    desc (a :: a) = ["a = " ++ show a]
    lhsS = "segAct mempty a"
    lhs = segAct (mempty @f)
    rhsS = "a"
    rhs = id

segActMonoidAction :: forall f a. (SegAct f a, QC.Arbitrary f, Eq f, Show f, Monoid a, Eq a, QC.Arbitrary a, Show a) => Proxy (f, a) -> QC.Property
segActMonoidAction _ = myForAllShrink True (const True) desc lhsS lhs rhsS rhs
  where
    desc :: (f, f, a) -> [String]
    desc (!f2, !f1, !a) = ["f2 = " ++ show f2 ++ ", f1 = " ++ show f1 ++ ", a = " ++ show a]
    lhsS = "(f_2 <> f_1) a"
    lhs (!f2, !f1, !a) = (f2 <> f1) `segAct` a
    rhsS = "f_2 (f_1 a)"
    rhs (!f2, !f1, !a) = f2 `segAct` (f1 `segAct` a)

segActEndomorphism :: forall f a. (SegAct f a, QC.Arbitrary f, Eq f, Show f, Monoid a, Eq a, QC.Arbitrary a, Show a) => Proxy (f, a) -> QC.Property
segActEndomorphism _ = myForAllShrink True (const True) desc lhsS lhs rhsS rhs
  where
    desc :: (f, a, a) -> [String]
    desc (!f, !a1, !a2) = ["f = " ++ show f ++ ", a1 = " ++ show a1 ++ ", a2 = " ++ show a2]
    lhsS = "f (a1 <> a2)"
    lhs (!f, !a1, !a2) = segActWithLength 2 f (a1 <> a2)
    rhsS = "(f a1) <> (f a2)"
    rhs (!f, !a1, !a2) = (f `segAct` a1) <> (f `segAct` a2)

segActLinearMonoidAction :: forall f a. (SegAct f a, QC.Arbitrary f, Eq f, Show f, Monoid a, Eq a, QC.Arbitrary a, Show a) => Proxy (f, a) -> QC.Property
segActLinearMonoidAction _ = myForAllShrink True (const True) desc lhsS lhs rhsS rhs
  where
    desc :: (QC.Positive Int, f, a) -> [String]
    desc (QC.Positive !len, !f, !a) = ["len = " ++ show len ++ ", f = " ++ show f ++ ", a = " ++ show a]
    lhsS = "segActWithLength len f (a^len)"
    lhs (QC.Positive !len, !f, !a) = segActWithLength len f $! stimes len a
    rhsS = "(f a)^len"
    rhs (QC.Positive !len, !f, !a) = stimes len (segAct f a)

-- orphan instance
instance (QC.Arbitrary a, Monoid a) => QC.Arbitrary (Affine1 a) where
  arbitrary = Affine1 <$> QC.arbitrary

-- orphan instance
instance (QC.Arbitrary a) => QC.Arbitrary (RangeAdd a) where
  arbitrary = RangeAdd <$> QC.arbitrary

-- orphan instance
instance (QC.Arbitrary a, Monoid a) => QC.Arbitrary (RangeSet a) where
  arbitrary = do
    b <- (== 1) <$> QC.chooseInt (1, 30)
    if b
      then RangeSet . (Bit True,) <$> QC.arbitrary
      else pure mempty

-- orphan instance
instance (QC.Arbitrary a) => QC.Arbitrary (Mat2x2 a) where
  arbitrary = Mat2x2 <$> QC.arbitrary

-- orphan instance
instance (QC.Arbitrary a) => QC.Arbitrary (V2 a) where
  arbitrary = V2 <$> QC.arbitrary

-- orphan instance
instance QC.Arbitrary (Max Int) where
  arbitrary = Max <$> QC.arbitrary

-- orphan instance
instance QC.Arbitrary (Min Int) where
  arbitrary = Min <$> QC.arbitrary

prop_affineZero :: Affine1 (Sum Int) -> QC.Property
prop_affineZero a =
  QC.conjoin
    [ A.zero <> a QC.=== A.zero,
      a <> A.zero QC.=== (\(A.Affine1 (!_, !b)) -> A.Affine1 (0, b)) a
    ]

tests :: [TestTree]
tests =
  [ testGroup
      "Affine1"
      [ QC.testProperty "zero" prop_affineZero,
        laws @(Affine1 (Sum Int))
          [ QCC.semigroupLaws,
            QCC.monoidLaws,
            QCC.semigroupMonoidLaws
          ],
        laws @(Affine1 (Sum Int), Sum Int)
          [ segActLaw
          ]
      ],
    testGroup
      "RangeAdd"
      [ laws @(RangeAdd (Sum Int))
          [ QCC.semigroupLaws,
            QCC.monoidLaws,
            QCC.semigroupMonoidLaws
          ],
        laws @(RangeAdd (Sum Int), Sum Int)
          [ segActLaw
          ],
        laws @(RangeAdd (Max Int))
          [ QCC.semigroupLaws,
            QCC.monoidLaws,
            QCC.semigroupMonoidLaws
          ],
        laws @(RangeAdd (Max Int), Max Int)
          [ segActLaw
          ],
        laws @(RangeAdd (Min Int))
          [ QCC.semigroupLaws,
            QCC.monoidLaws,
            QCC.semigroupMonoidLaws
          ],
        laws @(RangeAdd (Min Int), Min Int)
          [ segActLaw
          ]
      ],
    testGroup
      "RangeSet"
      [ laws @(RangeSet (Sum Int))
          [ QCC.semigroupLaws,
            QCC.monoidLaws,
            QCC.semigroupMonoidLaws
          ],
        laws @(RangeSet (Sum Int), Sum Int)
          [ segActLaw
          ],
        laws @(RangeSet (Product Int))
          [ QCC.semigroupLaws,
            QCC.monoidLaws,
            QCC.semigroupMonoidLaws
          ],
        laws @(RangeSet (Product Int), Product Int)
          [ segActLaw
          ],
        laws @(RangeSet (Max Int))
          [ QCC.semigroupLaws,
            QCC.monoidLaws,
            QCC.semigroupMonoidLaws
          ],
        laws @(RangeSet (Max Int), Max Int)
          [ segActLaw
          ],
        laws @(RangeSet (Min Int))
          [ QCC.semigroupLaws,
            QCC.monoidLaws,
            QCC.semigroupMonoidLaws
          ],
        laws @(RangeSet (Min Int), Min Int)
          [ segActLaw
          ]
      ],
    testGroup
      "Mat2x2"
      [ laws @(Mat2x2 Int)
          [ QCC.semigroupLaws,
            QCC.monoidLaws,
            QCC.semigroupMonoidLaws
          ],
        laws @(Mat2x2 Int, V2 Int)
          [ segActLaw
          ]
      ],
    testGroup
      "V2"
      [ laws @(V2 Int)
          [ QCC.semigroupLaws,
            QCC.monoidLaws,
            QCC.semigroupMonoidLaws
          ]
      ]
  ]
