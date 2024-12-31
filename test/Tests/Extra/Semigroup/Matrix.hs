module Tests.Extra.Semigroup.Matrix (tests) where

import AtCoder.Extra.Semigroup.Matrix qualified as Mat
import AtCoder.ModInt qualified as M
import Data.Vector.Unboxed qualified as VU
import GHC.TypeNats (KnownNat)
import Test.QuickCheck.Classes qualified as QCC
import Test.Tasty
import Test.Tasty.QuickCheck qualified as QC
import Tests.Util (laws)

-- TODO: (const True) should be removed

-- orphan instance
instance (QC.Arbitrary a, VU.Unbox a) => QC.Arbitrary (Mat.Matrix a) where
  -- for simplicity, make a 33x33 matrix
  arbitrary = do
    let n = 33
    vec <- VU.fromList <$> QC.vectorOf (n * n) (QC.arbitrary @a)
    pure $ Mat.Matrix n n vec

-- orphan instance
instance (KnownNat p) => QC.Arbitrary (M.ModInt p) where
  arbitrary = M.new <$> QC.arbitrary

prop_mulToCol :: QC.Gen QC.Property
prop_mulToCol = do
  h <- QC.chooseInt (1, 16)
  w <- QC.chooseInt (1, 16)
  vec <- VU.fromList <$> QC.vectorOf (h * w) (QC.arbitrary @Int)
  let mat = Mat.new h w vec
  col <- VU.fromList <$> QC.vectorOf w (QC.arbitrary @Int)
  let lhs = Mat.mulToCol mat col
  let rhs = Mat.vecM $ Mat.mul mat (Mat.new w 1 col)
  pure  $ lhs QC.=== rhs

tests :: [TestTree]
tests =
  [ QC.testProperty "mulToCol" prop_mulToCol,
    laws @(Mat.Matrix (M.ModInt 998244353))
      [ QCC.semigroupLaws
      ]
  ]