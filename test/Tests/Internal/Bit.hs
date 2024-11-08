{-# LANGUAGE RecordWildCards #-}

module Tests.Internal.Bit (tests) where

import AtCoder.Internal.Bit qualified as ACIBIT
import Test.Tasty
import Test.Tasty.HUnit
import Data.Bits ((.<<.), countTrailingZeros)

unit_bitCeil :: TestTree
unit_bitCeil = testCase "bitCeil" $ do
  1 @=? ACIBIT.bitCeil 0
  1 @=? ACIBIT.bitCeil 1
  2 @=? ACIBIT.bitCeil 2
  4 @=? ACIBIT.bitCeil 3
  4 @=? ACIBIT.bitCeil 4
  8 @=? ACIBIT.bitCeil 5
  8 @=? ACIBIT.bitCeil 6
  8 @=? ACIBIT.bitCeil 7
  8 @=? ACIBIT.bitCeil 8
  16 @=? ACIBIT.bitCeil 9
  (1 .<<. 30) @=? ACIBIT.bitCeil (1 .<<. 30)
  (1 .<<. 31) @=? ACIBIT.bitCeil ((1 .<<. 30) + 1)
  (1 .<<. 31) @=? ACIBIT.bitCeil ((1 .<<. 31) - 1)
  (1 .<<. 31) @=? ACIBIT.bitCeil (1 .<<. 31)
  (1 .<<. 61) @=? ACIBIT.bitCeil (1 .<<. 61)
  (1 .<<. 62) @=? ACIBIT.bitCeil ((1 .<<. 61) + 1)
  (1 .<<. 62) @=? ACIBIT.bitCeil ((1 .<<. 62) - 1)
  (1 .<<. 62) @=? ACIBIT.bitCeil (1 .<<. 62)

unit_countrZero :: TestTree
unit_countrZero = testCase "coutrZero" $ do
  0 @=? countTrailingZeros (1 :: Int)
  1 @=? countTrailingZeros (2 :: Int)
  0 @=? countTrailingZeros (3 :: Int)
  2 @=? countTrailingZeros (4 :: Int)
  0 @=? countTrailingZeros (5 :: Int)
  1 @=? countTrailingZeros (6 :: Int)
  0 @=? countTrailingZeros (7 :: Int)
  3 @=? countTrailingZeros (8 :: Int)
  0 @=? countTrailingZeros (9 :: Int)
  30 @=? countTrailingZeros (1 .<<. 30 :: Int)
  0 @=? countTrailingZeros ((1 .<<. 31) - 1 :: Int)
  31 @=? countTrailingZeros (1 .<<. 31 :: Int)
  0 @=? countTrailingZeros (maxBound @Int)

-- TEST(BitTest, CountrZeroConstexpr)

tests :: [TestTree]
tests = [unit_bitCeil, unit_countrZero]
