{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}

module Tests.ModInt (tests) where

import AtCoder.ModInt qualified as ModInt
import Control.Exception (evaluate)
import Control.Monad (when)
import Data.Bits
import Data.Bits ((.<<.))
import Data.Foldable
import Data.Proxy (Proxy (..))
import Data.WideWord (Int128, Word128)
import Data.Word (Word32)
import GHC.Exts (proxy#)
import GHC.TypeLits (KnownNat, natVal, natVal')
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck qualified as QC

-- TODO: the tests are not enough?

-- | Orphan `Modulus` instance for hunit tests.
instance ModInt.Modulus 1 where
  isPrimeModulus _ = False

instance ModInt.Modulus 11 where
  isPrimeModulus _ = False

instance ModInt.Modulus 12 where
  isPrimeModulus _ = False

instance ModInt.Modulus 1000 where
  isPrimeModulus _ = False

instance ModInt.Modulus 1_000_000_008 where
  isPrimeModulus _ = False

instance ModInt.Modulus 2147483647 where
  isPrimeModulus _ = True

unit_modulus :: TestTree
unit_modulus = testCase "modulus" $ do
  (@?= 998244353) $ ModInt.modulus (999 :: ModInt.ModInt998244353)
  (@?= 1000000007) $ ModInt.modulus (999 :: ModInt.ModInt1000000007)

unit_mod1 :: TestTree
unit_mod1 = testCase "mod1" $ do
  let modInt :: Int -> ModInt.StaticModInt 1
      modInt = ModInt.new

  for_ [0 .. 100 - 1] $ \i -> do
    for_ [0 .. 100 - 1] $ \j -> do
      modInt i * modInt j @?= 0

  modInt 1234 + modInt 5678 @?= 0
  modInt 1234 - modInt 5678 @?= 0
  modInt 1234 * modInt 5678 @?= 0
  ModInt.pow (modInt 1234) 5678 @?= 0
  ModInt.inv (modInt 0) @?= 0

unit_intMax :: TestTree
unit_intMax = testCase "intMax" $ do
  let modInt :: Int -> ModInt.StaticModInt 2147483647
      modInt = ModInt.new

  for_ [0 .. 100 - 1] $ \i -> do
    for_ [0 .. 100 - 1] $ \j -> do
      ModInt.val (modInt i * modInt j) @=? i * j

  modInt 1234 + modInt 5678 @?= 1234 + 5678
  modInt 1234 - modInt 5678 @?= 2147483647 - 5678 + 1234
  modInt 1234 * modInt 5678 @?= 1234 * 5678
  modInt 2147483647 * modInt 2147483647 @?= 0

unit_int128 :: TestTree
unit_int128 = testCase "intMax" $ do
  let modInt :: Int -> ModInt.ModInt998244353
      modInt = ModInt.new

  12345678 @=? ModInt.val (fromIntegral @_ @ModInt.ModInt998244353 (12345678 :: Int128))
  12345678 @=? ModInt.val (fromIntegral @_ @ModInt.ModInt998244353 (12345678 :: Word128))
  ModInt.val (ModInt.pow (modInt 2) 100) @=? ModInt.val (fromIntegral @_ @ModInt.ModInt998244353 ((1 :: Int128) .<<. 100))
  ModInt.val (ModInt.pow (modInt 2) 100) @=? ModInt.val (fromIntegral @_ @ModInt.ModInt998244353 ((1 :: Word128) .<<. 100))

unit_inv :: TestTree
unit_inv = testCase "inv" $ do
  for_ [1 .. 10 - 1] $ \i -> do
    1 @=? (ModInt.val (ModInt.inv (ModInt.new @11 i)) * i) `rem` 11

  for_ [1 .. 11 - 1] $ \i -> do
    when (gcd i 12 == 1) $ do
      1 @=? (ModInt.val (ModInt.inv (ModInt.new @12 i)) * i) `rem` 12

  for_ [1 .. 100000 - 1] $ \i -> do
    1 @=? (ModInt.val (ModInt.inv (ModInt.new @1_000_000_007 i)) * i) `rem` 1_000_000_007

  for_ [1 .. 100000 - 1] $ \i -> do
    when (gcd i 1_000_000_008 == 1) $ do
      1 @=? (ModInt.val (ModInt.inv (ModInt.new @1_000_000_008 i)) * i) `rem` 1_000_000_008

-- ConstUsage

unit_increment :: TestTree
unit_increment = testCase "increment" $ do
  let modInt :: Int -> ModInt.StaticModInt 11
      modInt = ModInt.new

  -- not incrementations though
  let a = modInt 8
  9 @=? a + 1
  10 @=? a + 2
  0 @=? a + 3
  1 @=? a + 4

  let b = modInt 3
  2 @=? b - 1
  1 @=? b - 2
  0 @=? b - 3
  10 @=? b - 4

spec_staticUsage :: IO TestTree
spec_staticUsage = testSpec "staticUsage" $ do
  let modInt :: Int -> ModInt.StaticModInt 11
      modInt = ModInt.new

  it "ok" $ do
    11 `shouldBe` ModInt.modulus (modInt 0)
    11 `shouldBe` ModInt.modVal (Proxy @11)
    11 `shouldBe` ModInt.modVal# (proxy# @11)

  it "ok" $ modInt 1 /= modInt 3
  it "ok" $ modInt 1 == modInt 12

  it "throws error" $ do
    evaluate (ModInt.pow (modInt 3) (-1)) `shouldThrow` anyException

unit_constructorStatic :: TestTree
unit_constructorStatic = testCase "constructorStatic" $ do
  let modInt :: Int -> ModInt.StaticModInt 11
      modInt = ModInt.new

  1 @=? ModInt.val (modInt (fromEnum True))
  0 @=? ModInt.val (modInt 0)

-- | Orphan `Arbitrary` instance for QuickCheck tests.
instance (ModInt.Modulus a) => QC.Arbitrary (ModInt.StaticModInt a) where
  arbitrary = ModInt.new <$> QC.arbitrary

prop_new :: ModInt.ModInt998244353 -> Bool
prop_new x =
  let r = ModInt.val x
   in 0 <= r && r < 998244353

prop_primeMul :: ModInt.ModInt998244353 -> ModInt.ModInt998244353 -> ModInt.ModInt998244353 -> Bool
prop_primeMul x y c = (x + y) * c == (x * c + y * c)

prop_primeInv :: ModInt.ModInt998244353 -> Bool
prop_primeInv x
  | x == 0 = True
  | otherwise = ModInt.inv x * x == 1 && x * ModInt.inv x == 1

prop_nonPrimeMul :: ModInt.ModInt998244353 -> ModInt.ModInt998244353 -> ModInt.ModInt998244353 -> Bool
prop_nonPrimeMul x y c = (x + y) * c == (x * c + y * c)

prop_nonPrimeInv :: ModInt.ModInt998244353 -> Bool
prop_nonPrimeInv x = x - x == 0

-- bitwise operators

-- | Generator of `Int` in @[0, 31]@.
newtype WordBit = WordBit Int
  deriving newtype (Eq, Show, Ord)

instance QC.Arbitrary WordBit where
  arbitrary = WordBit <$> QC.chooseInt (0, finiteBitSize (0 :: Word32) - 1)

wrap :: (KnownNat p) => (forall a. (Bits a) => a -> a) -> ModInt.StaticModInt p -> Bool
wrap op a@(ModInt.StaticModInt a') =
  fromIntegral (ModInt.val (op a)) == op a' `mod` fromIntegral (ModInt.modulus a)

wrap1 :: (KnownNat p, QC.Arbitrary b) => (forall a. (Bits a) => a -> b -> a) -> ModInt.StaticModInt p -> b -> Bool
wrap1 op a@(ModInt.StaticModInt a') b =
  fromIntegral (ModInt.val (op a b)) == op a' b `mod` fromIntegral (ModInt.modulus a)

wrap2 :: (KnownNat p) => (forall a. (Bits a) => a -> a -> a) -> ModInt.StaticModInt p -> ModInt.StaticModInt p -> Bool
wrap2 op a@(ModInt.StaticModInt a') b@(ModInt.StaticModInt b') =
  fromIntegral (ModInt.val (op a b)) == op a' b' `mod` fromIntegral (ModInt.modulus a)

wrapInt :: (KnownNat p) => (forall a. (Bits a) => a -> Int -> a) -> ModInt.StaticModInt p -> Int -> Bool
wrapInt op a@(ModInt.StaticModInt a') i =
  fromIntegral (ModInt.val (op a i)) == op a' i `mod` fromIntegral (ModInt.modulus a)

wrapBit :: (KnownNat p) => (forall a. (Bits a) => a -> Int -> a) -> ModInt.StaticModInt p -> WordBit -> Bool
wrapBit op a@(ModInt.StaticModInt a') (WordBit i) =
  fromIntegral (ModInt.val (op a i)) == op a' i `mod` fromIntegral (ModInt.modulus a)

wrapPositive :: (KnownNat p) => (forall a. (Bits a) => a -> Int -> a) -> ModInt.StaticModInt p -> QC.Positive Int -> Bool
wrapPositive op a@(ModInt.StaticModInt a') (QC.Positive i) =
  fromIntegral (ModInt.val (op a i)) == op a' i `mod` fromIntegral (ModInt.modulus a)

prop_bitAnd :: ModInt.ModInt998244353 -> ModInt.ModInt998244353 -> Bool
prop_bitAnd = wrap2 (.&.)

prop_bitOr :: ModInt.ModInt998244353 -> ModInt.ModInt998244353 -> Bool
prop_bitOr = wrap2 (.|.)

prop_bitXor :: ModInt.ModInt998244353 -> ModInt.ModInt998244353 -> Bool
prop_bitXor = wrap2 (.|.)

prop_complement :: ModInt.ModInt998244353 -> Bool
prop_complement = wrap complement

prop_shift :: ModInt.ModInt998244353 -> Int -> Bool
prop_shift = wrapInt shift

prop_rotate :: ModInt.ModInt998244353 -> Int -> Bool
prop_rotate = wrapInt rotate

prop_zeroBits :: Bool
prop_zeroBits = zeroBits @ModInt.ModInt998244353 == zeroBits

prop_bit :: WordBit -> Bool
prop_bit (WordBit i) = ModInt.val32 @998244353 (bit i) == ((bit i :: Word32) `mod` 998244353)

prop_setBit :: ModInt.ModInt998244353 -> WordBit -> Bool
prop_setBit = wrapBit setBit

prop_clearBit :: ModInt.ModInt998244353 -> WordBit -> Bool
prop_clearBit = wrapBit clearBit

prop_compelementBit :: ModInt.ModInt998244353 -> QC.Positive Int -> Bool
prop_compelementBit = wrapPositive complementBit

prop_testBit :: ModInt.ModInt998244353 -> QC.Positive Int -> Bool
prop_testBit a@(ModInt.StaticModInt a') (QC.Positive i) = testBit a i == testBit a' i

-- TOOD: test, or define their proper behavior
-- prop_bitSizeMaybe :: ModInt.ModInt998244353 -> Bool
-- prop_bitSize :: ModInt.ModInt998244353 -> Bool

prop_isSigned :: ModInt.ModInt998244353 -> Bool
prop_isSigned a@(ModInt.StaticModInt a') = isSigned a == isSigned a'

prop_testShiftL :: ModInt.ModInt998244353 -> Int -> Bool
prop_testShiftL = wrapInt shiftL

prop_testUnsafeShiftL :: ModInt.ModInt998244353 -> Int -> Bool
prop_testUnsafeShiftL = wrapInt unsafeShiftL

prop_testShiftR :: ModInt.ModInt998244353 -> Int -> Bool
prop_testShiftR = wrapInt shiftR

prop_testUnsafeShiftR :: ModInt.ModInt998244353 -> Int -> Bool
prop_testUnsafeShiftR = wrapInt unsafeShiftR

prop_testRotateL :: ModInt.ModInt998244353 -> Int -> Bool
prop_testRotateL = wrapInt rotateL

prop_testRotateR :: ModInt.ModInt998244353 -> Int -> Bool
prop_testRotateR = wrapInt rotateR

prop_testPopCount :: ModInt.ModInt998244353 -> Bool
prop_testPopCount a@(ModInt.StaticModInt a') = popCount a == popCount a'

tests :: [TestTree]
tests =
  [ unit_modulus,
    unit_mod1,
    unit_intMax,
    unit_int128,
    unit_inv,
    unit_increment,
    unit_constructorStatic,
    unsafePerformIO spec_staticUsage,
    QC.testProperty "prop_new" prop_new,
    QC.testProperty "prop_primeMul" prop_primeMul,
    QC.testProperty "prop_primeInv" prop_primeInv,
    QC.testProperty "prop_nonPrimeMul" prop_nonPrimeMul,
    QC.testProperty "prop_nonPrimeInv" prop_nonPrimeInv,
    QC.testProperty "prop_bitAnd" prop_bitAnd,
    QC.testProperty "prop_bitOr" prop_bitOr,
    QC.testProperty "prop_bitXor" prop_bitXor,
    QC.testProperty "prop_complement" prop_complement,
    QC.testProperty "prop_shift" prop_shift,
    QC.testProperty "prop_rotate" prop_rotate,
    QC.testProperty "prop_zeroBits" prop_zeroBits,
    QC.testProperty "prop_bit" prop_bit,
    QC.testProperty "prop_setBit" prop_setBit,
    QC.testProperty "prop_clearBit" prop_clearBit,
    QC.testProperty "prop_compelementBit" prop_compelementBit,
    QC.testProperty "prop_testBit" prop_testBit,
    -- prop_bitSizeMaybe,
    -- prop_bitSize,
    QC.testProperty "prop_isSigned" prop_isSigned,
    QC.testProperty "prop_testShiftL" prop_testShiftL,
    QC.testProperty "prop_testUnsafeShiftL" prop_testUnsafeShiftL,
    QC.testProperty "prop_testShiftR" prop_testShiftR,
    QC.testProperty "prop_testUnsafeShiftR" prop_testUnsafeShiftR,
    QC.testProperty "prop_testRotateL" prop_testRotateL,
    QC.testProperty "prop_testRotateR" prop_testRotateR,
    QC.testProperty "prop_testPopCount" prop_testPopCount
  ]
