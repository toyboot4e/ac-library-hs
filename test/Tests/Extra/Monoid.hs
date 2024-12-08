module Tests.Extra.Monoid (tests) where

import AtCoder.Extra.Monoid
import Data.Proxy (Proxy (..))
import Data.Semigroup (stimes, Max (..), Sum (..))
import Data.Typeable (Typeable, typeRep)
import Test.QuickCheck.Classes qualified as QCC
import Test.QuickCheck.Classes.Internal qualified as QCCI
import Test.Tasty
import Test.Tasty.QuickCheck qualified as QC

laws :: forall a. (Typeable a) => [Proxy a -> QCC.Laws] -> TestTree
laws =
  testGroup (show (typeRep (Proxy @a)))
    . map
      ( \f ->
          let QCC.Laws name pairs = f (Proxy @a)
           in testGroup name (map (uncurry QC.testProperty) pairs)
      )

segActLaw :: (Monoid a, Eq a) => (SegAct f a, QC.Arbitrary f, Eq f, Show f, QC.Arbitrary f, QC.Arbitrary a, Show a) => Proxy (f, a) -> QCC.Laws
segActLaw p =
  QCC.Laws
    "SegAct"
    [ ("Identity", segActIdentity p),
      ("Monoid Action", segActMonoidAction p),
      ("Linear Monoid Action", segActLinearMonoidAction p),
      ("Endomorphism", segActEndomorphism p)
    ]

segActIdentity :: forall f a. (SegAct f a, QC.Arbitrary f, Eq f, Show f, Monoid a, Eq a, QC.Arbitrary a, Show a) => Proxy (f, a) -> QC.Property
segActIdentity _ = QCCI.myForAllShrink True (const True) desc lhsS lhs rhsS rhs
  where
    desc :: a -> [String]
    desc (a :: a) = ["a = " ++ show a]
    lhsS = "segAct mempty a"
    lhs = segAct (mempty @f)
    rhsS = "a"
    rhs = id

segActMonoidAction :: forall f a. (SegAct f a, QC.Arbitrary f, Eq f, Show f, Monoid a, Eq a, QC.Arbitrary a, Show a) => Proxy (f, a) -> QC.Property
segActMonoidAction _ = QCCI.myForAllShrink True (const True) desc lhsS lhs rhsS rhs
  where
    desc :: (f, f, a) -> [String]
    desc (!f2, !f1, !a) = ["f2 = " ++ show f2 ++ " f1 = " ++ show f1 ++ " a = " ++ show a]
    lhsS = "(f_2 <> f_1) a"
    lhs (!f2, !f1, !a) = (f2 <> f1) `segAct` a
    rhsS = "f_2 (f_1 a)"
    rhs (!f2, !f1, !a) = f2 `segAct` (f1 `segAct` a)

segActEndomorphism :: forall f a. (SegAct f a, QC.Arbitrary f, Eq f, Show f, Monoid a, Eq a, QC.Arbitrary a, Show a) => Proxy (f, a) -> QC.Property
segActEndomorphism _ = QCCI.myForAllShrink True (const True) desc lhsS lhs rhsS rhs
  where
    desc :: (f, a, a) -> [String]
    desc (!f, !a1, !a2) = ["f = " ++ show f ++ " a1 = " ++ show a1 ++ " a2 = " ++ show a2]
    lhsS = "f (a1 <> a2)"
    lhs (!f, !a1, !a2) = segActWithLength 2 f (a1 <> a2)
    rhsS = "(f a1) <> (f a2)"
    rhs (!f, !a1, !a2) = (f `segAct` a1) <> (f `segAct` a2)

segActLinearMonoidAction :: forall f a. (SegAct f a, QC.Arbitrary f, Eq f, Show f, Monoid a, Eq a, QC.Arbitrary a, Show a) => Proxy (f, a) -> QC.Property
segActLinearMonoidAction _ = QCCI.myForAllShrink True (const True) desc lhsS lhs rhsS rhs
  where
    desc :: (QC.Positive Int, f, a) -> [String]
    desc (QC.Positive !len, !f, !a) = ["len = " ++ show len ++ " f = " ++ show f ++ " a = " ++ show a]
    lhsS = "segActWithLength len f (a^3)"
    lhs (QC.Positive !len, !f, !a) = segActWithLength len f $! stimes len a
    rhsS = "(f a)^len"
    rhs (QC.Positive !len, !f, !a) = stimes len (segAct f a)

-- orphan instance
instance (QC.Arbitrary a, Monoid a) => QC.Arbitrary (Affine1 a) where
  arbitrary = Affine1 <$> QC.arbitrary

-- orphan instance
instance (QC.Arbitrary a, Monoid a) => QC.Arbitrary (RangeSet a) where
  arbitrary = do
    b <- (== 1) <$> QC.chooseInt (1, 30)
    if b
      then RangeSet . (True,) <$> QC.arbitrary
      else pure mempty

-- orphan instance
instance QC.Arbitrary (RangeSetId (Max Int)) where
  arbitrary = do
    b <- (== 1) <$> QC.chooseInt (1, 30)
    if b
      then RangeSetId . (True,) . Max <$> QC.arbitrary
      else pure mempty

-- orphan instance
instance QC.Arbitrary (Max Int) where
  arbitrary = Max <$> QC.arbitrary

tests :: [TestTree]
tests =
  [ testGroup
      "Affine1"
      [ laws @(Affine1 (Sum Int))
          [ QCC.semigroupLaws,
            QCC.monoidLaws,
            QCC.semigroupMonoidLaws
          ],
        laws @(Affine1 (Sum Int), Sum Int)
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
          ]
      ],
    testGroup
      "RangeSetId"
      [ laws @(RangeSetId (Max Int))
          [ QCC.semigroupLaws,
            QCC.idempotentSemigroupLaws,
            QCC.monoidLaws,
            QCC.semigroupMonoidLaws
          ],
        laws @(RangeSetId (Max Int), Max Int)
          [ segActLaw
          ]
      ]
  ]
