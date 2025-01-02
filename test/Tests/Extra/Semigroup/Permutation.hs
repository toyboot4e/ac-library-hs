module Tests.Extra.Semigroup.Permutation (tests) where

import AtCoder.Extra.Semigroup.Permutation qualified as P
import Control.Exception (evaluate)
import Data.Vector.Unboxed qualified as VU
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec
import Test.QuickCheck.Classes qualified as QCC
import Test.Tasty
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck as QC
import Tests.Util (laws)

spec_invalid :: IO TestTree
spec_invalid = testSpec "boundary check" $ do
  let !_ = P.new $ VU.fromList [0]
  let !_ = P.new $ VU.fromList [-1] -- -1 is allowed
  it "throws error 1" $ do
    evaluate (P.new (VU.fromList [1])) `shouldThrow` anyException
  it "throws error 2" $ do
    evaluate (P.new (VU.fromList [-2])) `shouldThrow` anyException

prop_ident :: P.Permutation -> QC.Property
prop_ident p =
  QC.conjoin
    [ p <> ident QC.=== p,
      ident <> p QC.=== p
    ]
  where
    ident = P.ident (P.length p)

prop_zero :: P.Permutation -> QC.Property
prop_zero p =
  QC.conjoin
    [ p <> zero QC.=== zero,
      zero <> p QC.=== zero
    ]
  where
    zero = P.zero (P.length p)

-- orphan instance
instance QC.Arbitrary P.Permutation where
  arbitrary = do
    let n = 33
    vec <- VU.fromList <$> QC.vectorOf n (QC.chooseInt (-1, n - 1))
    pure $ P.new vec

tests :: [TestTree]
tests =
  [ unsafePerformIO spec_invalid,
    QC.testProperty "ident" prop_ident,
    QC.testProperty "zero" prop_zero,
    laws
      @P.Permutation
      [ QCC.semigroupLaws
      ]
  ]
