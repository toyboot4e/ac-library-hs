module Tests.Util (myForAllShrink, laws) where

import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable, typeRep)
import Test.QuickCheck.Classes qualified as QCC
import Test.QuickCheck.Property qualified as QC
import Test.Tasty
import Test.Tasty.QuickCheck qualified as QC

-- | Taken from `quickcheck-classes-base`.
myForAllShrink ::
  (QC.Arbitrary a, Show b, Eq b) =>
  Bool -> -- Should we show the RHS. It's better not to show it if the RHS is equal to the input.
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
