-- | A bit tedious tests.
module Tests.MulMod (tests) where

import BenchLib.MulMod.Barrett64 qualified as Barrett64
import BenchLib.MulMod.BarrettWideWord qualified as BarrettWideWord
import BenchLib.MulMod.Montgomery qualified as Montgomery
import Data.Foldable (for_)
import Data.Proxy (Proxy (..))
import Data.Word (Word64)
import GHC.TypeNats (KnownNat, natVal)
import Test.QuickCheck qualified as QC
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck qualified as QC

newtype InRange s a = InRange a
  deriving (Show, Eq)

instance (KnownNat s) => QC.Arbitrary (InRange s Word64) where
  arbitrary = InRange . (`mod` fromIntegral (natVal (Proxy @s))) <$> QC.arbitrary

prop_barrettWideWord :: forall m. (KnownNat m) => InRange m Word64 -> InRange m Word64 -> Bool
prop_barrettWideWord (InRange a) (InRange b) = a * b `mod` m == BarrettWideWord.mulMod bt a b
  where
    m = fromIntegral $ natVal (Proxy @m) :: Word64
    bt = BarrettWideWord.new64 m

unit_barrettWideWordBounds :: Word64 -> TestTree
unit_barrettWideWordBounds m = testCase ("barrett64 bounds " ++ show m) $ do
  let bt = BarrettWideWord.new64 m
  for_ [0 .. 4] $ \a -> do
    for_ [0 .. 4] $ \b -> do
      a * b `mod` m @=? BarrettWideWord.mulMod bt a b
  for_ [fromIntegral m - 5 .. fromIntegral m - 1] $ \a -> do
    for_ [fromIntegral m - 5 .. fromIntegral m - 1] $ \b -> do
      fromIntegral a * fromIntegral b `mod` m @=? BarrettWideWord.mulMod bt a b

prop_barrett64 :: forall m. (KnownNat m) => InRange m Word64 -> InRange m Word64 -> Bool
prop_barrett64 (InRange a) (InRange b) = (a * b `mod` m) == Barrett64.mulMod bt a b
  where
    m = fromIntegral $ natVal (Proxy @m)
    bt = Barrett64.new m

unit_barrett64Bounds :: Word64 -> TestTree
unit_barrett64Bounds m = testCase ("barrett64 bounds " ++ show m) $ do
  let bt = Barrett64.new m
  for_ [0 .. 4] $ \a -> do
    for_ [0 .. 4] $ \b -> do
      a * b `mod` m @=? Barrett64.mulMod bt a b
  for_ [m - 5 .. m - 1] $ \a -> do
    for_ [m - 5 .. m - 1] $ \b -> do
      a * b `mod` m @=? Barrett64.mulMod bt a b

prop_montgomery :: forall m. (KnownNat m) => InRange m Word64 -> InRange m Word64 -> Bool
prop_montgomery (InRange a) (InRange b) = a * b `mod` m == Montgomery.reduce mont (Montgomery.mulMod mont a b)
  where
    m = fromIntegral $ natVal (Proxy @m)
    mont = Montgomery.new m

unit_montgomeryBounds :: Word64 -> TestTree
unit_montgomeryBounds m = testCase ("montgomery64 bounds " ++ show m) $ do
  let mont = Montgomery.new m
  for_ [0 .. 4] $ \a -> do
    for_ [0 .. 4] $ \b -> do
      a * b `mod` m @=? Montgomery.reduce mont (Montgomery.mulMod mont a b)
  for_ [m - 4 .. m - 1] $ \a -> do
    for_ [m - 4 .. m - 1] $ \b -> do
      a * b `mod` m @=? Montgomery.reduce mont (Montgomery.mulMod mont a b)

tests :: [TestTree]
tests =
  [ QC.testProperty "barrettWideWord random 998244353" $ prop_barrettWideWord @998244353,
    unit_barrettWideWordBounds 998244353,
    QC.testProperty "barrett64 random 998244353" $ prop_barrett64 @998244353,
    unit_barrett64Bounds 998244353,
    QC.testProperty "montgomery random 998244353" $ prop_montgomery @998244353,
    unit_montgomeryBounds 998244353,
    -- TODO: run in a separate tree
    QC.testProperty "barrettWideWord random 2147483647" $ prop_barrettWideWord @2147483647,
    unit_barrettWideWordBounds 2147483647,
    QC.testProperty "barrett64 random 2147483647" $ prop_barrett64 @2147483647,
    unit_barrett64Bounds 2147483647,
    QC.testProperty "montgomery random 2147483647" $ prop_montgomery @2147483647,
    unit_montgomeryBounds 2147483647
  ]
