module Tests.Extra.Monoid (tests) where

import AtCoder.Extra.Monoid
import Data.Proxy (Proxy (..))
import Data.Semigroup (Max (..), Min (..), Product (..), Sum (..), stimes)
import Data.Typeable (Typeable, typeRep)
import Test.QuickCheck.Classes qualified as QCC
import Test.QuickCheck.Property qualified as QC
import Test.Tasty
import Test.Tasty.QuickCheck qualified as QC

-- | Taken from `quickcheck-classes-base`.
myForAllShrink ::
  (QC.Arbitrary a, Show b, Eq b) =>
  Bool -> -- Should we show the RHS. It's better not to show it
  -- if the RHS is equal to the input.
  (a -> Bool) -> -- is the value a valid input
  (a -> [String]) -> -- show the 'a' values
  String -> -- show the LHS
  (a -> b) -> -- the function that makes the LHS
  String -> -- show the RHS
  (a -> b) -> -- the function that makes the RHS
  QC.Property
myForAllShrink displayRhs isValid showInputs name1 calc1 name2 calc2 =
  QC.MkProperty $
    QC.arbitrary >>= \x ->
      QC.unProperty $
        QC.shrinking QC.shrink x $ \x' ->
          let b1 = calc1 x'
              b2 = calc2 x'
              sb1 = show b1
              sb2 = show b2
              description = "  Description: " ++ name1 ++ " = " ++ name2
              err = description ++ "\n" ++ unlines (map ("  " ++) (showInputs x')) ++ "  " ++ name1 ++ " = " ++ sb1 ++ (if displayRhs then "\n  " ++ name2 ++ " = " ++ sb2 else "")
           in isValid x' QC.==> QC.counterexample err (b1 == b2)

-- | Taken from `quickcheck-classes-base`.
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
instance (QC.Arbitrary a) => QC.Arbitrary (RangeAddId a) where
  arbitrary = RangeAddId <$> QC.arbitrary

-- orphan instance
instance (QC.Arbitrary a, Monoid a) => QC.Arbitrary (RangeSet a) where
  arbitrary = do
    b <- (== 1) <$> QC.chooseInt (1, 30)
    if b
      then RangeSet . (True,) <$> QC.arbitrary
      else pure mempty

-- orphan instance
instance (QC.Arbitrary a, Monoid a) => QC.Arbitrary (RangeSetId a) where
  arbitrary = do
    b <- (== 1) <$> QC.chooseInt (1, 30)
    if b
      then RangeSetId . (True,) <$> QC.arbitrary
      else pure mempty

-- orphan instance
instance QC.Arbitrary (Max Int) where
  arbitrary = Max <$> QC.arbitrary

-- orphan instance
instance QC.Arbitrary (Min Int) where
  arbitrary = Min <$> QC.arbitrary

tests :: [TestTree]
tests =
  [ testGroup
      "Affine1"
      [ laws @(Affine1 (Sum Int))
          [ QCC.semigroupLaws,
            QCC.monoidLaws,
            QCC.semigroupMonoidLaws
          ],
        laws @(Affine1 (Sum Int))
          [ QCC.semigroupLaws,
            QCC.monoidLaws,
            QCC.semigroupMonoidLaws
          ]
      ],
    testGroup
      "RangeAdd"
      [ laws @(RangeAdd Int)
          [ QCC.semigroupLaws,
            QCC.monoidLaws,
            QCC.semigroupMonoidLaws
          ],
        laws @(RangeAdd Int, Sum Int)
          [ segActLaw
          ]
      ],
    testGroup
      "RangeAddId"
      [ laws @(RangeAddId Int)
          [ QCC.semigroupLaws,
            QCC.monoidLaws,
            QCC.semigroupMonoidLaws
          ],
        laws @(RangeAddId Int, Max Int)
          [ segActLaw
          ],
        laws @(RangeAddId Int, Min Int)
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
        laws @(RangeSet (Product Int))
          [ QCC.semigroupLaws,
            QCC.monoidLaws,
            QCC.semigroupMonoidLaws
          ],
        laws @(RangeSet (Max Int))
          [ QCC.semigroupLaws,
            QCC.monoidLaws,
            QCC.semigroupMonoidLaws
          ],
        laws @(RangeSet (Min Int))
          [ QCC.semigroupLaws,
            QCC.monoidLaws,
            QCC.semigroupMonoidLaws
          ],
        laws @(RangeSet (Sum Int), Sum Int)
          [ segActLaw
          ],
        laws @(RangeSet (Product Int), Product Int)
          [ segActLaw
          ],
        laws @(RangeSet (Max Int), Max Int)
          [ segActLaw
          ],
        laws @(RangeSet (Min Int), Min Int)
          [ segActLaw
          ]
      ],
    testGroup
      "RangeSetId"
      [ laws @(RangeSetId (Sum Int))
          [ QCC.semigroupLaws,
            QCC.idempotentSemigroupLaws,
            QCC.monoidLaws,
            QCC.semigroupMonoidLaws
          ],
        laws @(RangeSetId (Product Int))
          [ QCC.semigroupLaws,
            QCC.idempotentSemigroupLaws,
            QCC.monoidLaws,
            QCC.semigroupMonoidLaws
          ],
        laws @(RangeSetId (Max Int))
          [ QCC.semigroupLaws,
            QCC.idempotentSemigroupLaws,
            QCC.monoidLaws,
            QCC.semigroupMonoidLaws
          ],
        laws @(RangeSetId (Min Int))
          [ QCC.semigroupLaws,
            QCC.idempotentSemigroupLaws,
            QCC.monoidLaws,
            QCC.semigroupMonoidLaws
          ],
        laws @(RangeSetId (Max Int), Max Int)
          [ segActLaw
          ],
        laws @(RangeSetId (Min Int), Min Int)
          [ segActLaw
          ]
      ]
  ]
