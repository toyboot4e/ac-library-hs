{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}

module Tests.ModInt (tests) where

import AtCoder.ModInt qualified as ModInt
import Control.Exception (evaluate)
import Control.Monad (when)
import Data.Bits ((.<<.))
import Data.Foldable
import Data.Proxy (Proxy (..))
import Data.WideWord (Int128, Word128)
import GHC.Exts (proxy#)
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck as QC

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
  modInt 1234 - modInt 5678 @?= 2147483647 - 1234 + 5678
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
      1 @=? (ModInt.val (ModInt.inv (ModInt.new @12 i)) * i) `rem` 11

  for_ [1 .. 100000 - 1] $ \i -> do
    1 @=? (ModInt.val (ModInt.inv (ModInt.new @1_000_000_007 i)) * i) `rem` 1_000_000_007

  for_ [1 .. 100000 - 1] $ \i -> do
    1 @=? (ModInt.val (ModInt.inv (ModInt.new @1_000_000_008 i)) * i) `rem` 1_000_000_008

-- ConstUsage

unit_increment :: TestTree
unit_increment = testCase "increment" $ do
  let modInt :: Int -> ModInt.StaticModInt 11
      modInt = ModInt.new

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
instance (ModInt.Modulus a) => Arbitrary (ModInt.StaticModInt a) where
  arbitrary = ModInt.new <$> arbitrary

prop_new :: ModInt.ModInt998244353 -> Bool
prop_new x =
  let r = ModInt.val x
   in 0 <= r && r < 998244353 - 1

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
    QC.testProperty "prop_nonPrimeInv" prop_nonPrimeInv
  ]
