module Tests.Internal.Math (tests) where

import AtCoder.Internal.Assert qualified as ACIA
import AtCoder.Internal.Barrett qualified as ACIBT
import AtCoder.Internal.Math qualified as ACIM
import Control.Monad (foldM, unless, when)
import Data.Foldable
import Data.Int (Int32)
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import Data.WideWord.Word128
import Data.Word (Word32, Word64)
import Test.Tasty
import Test.Tasty.HUnit

isPrimeNaive :: Int -> Bool
isPrimeNaive n
  | n < 0 = error "given negative value"
  | n == 0 || n == 1 = False
  | otherwise = all (\i -> n `mod` i /= 0) $ takeWhile (\x -> x * x <= n) [2 ..]

isPrimitiveRootNaive :: (HasCallStack) => Int -> Int -> Bool
isPrimitiveRootNaive m g
  | not (1 <= g && g < m) = error "invalid input"
  | otherwise = inner 1 1
  where
    inner i x
      | i > m - 2 =
          let !_ = ACIA.runtimeAssert (x' == 1) "isPrimitiveRootNaive"
           in True
      | x' == 1 = False
      | otherwise = inner (i + 1) x'
      where
        x' = (fromIntegral x :: Word64) * fromIntegral g `mod` fromIntegral m

unit_barrett :: TestTree
unit_barrett = testCase "barrett" $ do
  for_ [1 .. 100 :: Word64] $ \m -> do
    let bt = ACIBT.new64 m
    for_ [0 .. m - 1 :: Word64] $ \a -> do
      for_ [0 .. m - 1 :: Word64] $ \b -> do
        (a * b) `mod` m @=? ACIBT.mulMod bt a b

  let bt = ACIBT.new64 1
  0 @=? ACIBT.mulMod bt 0 0

testBarrettWithModulo :: Word32 -> Assertion
testBarrettWithModulo modUpper = do
  for_ [modUpper, modUpper - 1 .. modUpper - 20] $ \modulo32 -> do
    let modulo64 :: Word64 = fromIntegral modulo32
    let bt = ACIBT.new32 modulo32
    let v = VU.create $ do
          vec <- VUM.unsafeNew @_ @Word32 40
          for_ [0 .. 10 - 1 :: Word32] $ \i -> do
            VGM.write vec (4 * fromIntegral i + 0) i
            VGM.write vec (4 * fromIntegral i + 1) $ modulo32 - i
            VGM.write vec (4 * fromIntegral i + 2) $ modulo32 `div` 2 + i
            VGM.write vec (4 * fromIntegral i + 3) $ modulo32 `div` 2 - i
          pure vec
    VU.forM_ v $ \a -> do
      let a64 :: Word64 = fromIntegral a
      let expected = fromIntegral $ ((a64 * a64) `mod` modulo64 * a64) `mod` modulo64
      (expected @=?) . fromIntegral $ ACIBT.mulMod bt a64 (ACIBT.mulMod bt a64 a64)
      VU.forM_ v $ \b -> do
        let b64 :: Word64 = fromIntegral b
        (a64 * b64) `mod` modulo64 @=? ACIBT.mulMod bt a64 b64

unit_barrettIntBorder :: TestTree
unit_barrettIntBorder = testCase "barrettIntBobrder" $ do
  let modUpper :: Word32 = fromIntegral $ maxBound @Int32
  testBarrettWithModulo modUpper

unit_barrettWord32Border :: TestTree
unit_barrettWord32Border = testCase "barrettWord32Bobrder" $ do
  let modUpper = maxBound @Word32
  testBarrettWithModulo modUpper

unit_isPrime :: TestTree
unit_isPrime = testCase "isPrime" $ do
  (False @=?) $ ACIM.isPrime 121
  (False @=?) $ ACIM.isPrime $ 11 * 13
  (True @=?) $ ACIM.isPrime 1_000_000_007
  (False @=?) $ ACIM.isPrime 1_000_000_008
  (True @=?) $ ACIM.isPrime 1_000_000_009
  for_ [0 .. 10000] $ \i -> do
    isPrimeNaive i @=? ACIM.isPrime i
  for_ [0 .. 10000] $ \i -> do
    let x :: Int = fromIntegral $ maxBound @Int32 - i
    isPrimeNaive x @=? ACIM.isPrime x

-- SafeMod

unit_invGcdBound :: TestTree
unit_invGcdBound = testCase "invGcdBound" $ do
  let ps = VU.create $ do
        p <- VUM.unsafeNew @_ @Int (12 * 11 + 6)
        -- TODO: use `maxBound @Int` for Int variant of invGcd next time
        for_ [0 .. 10 :: Int] $ \i -> do
          VGM.write p (12 * i + 0) i
          VGM.write p (12 * i + 1) (-i)
          VGM.write p (12 * i + 2) $ minBound @Int + i
          VGM.write p (12 * i + 3) $ maxBound @Int - i

          VGM.write p (12 * i + 4) $ minBound @Int `div` 2 + i
          VGM.write p (12 * i + 5) $ minBound @Int `div` 2 - i
          VGM.write p (12 * i + 6) $ maxBound @Int `div` 2 + i
          VGM.write p (12 * i + 7) $ maxBound @Int `div` 2 - i

          VGM.write p (12 * i + 8) $ minBound @Int `div` 3 + i
          VGM.write p (12 * i + 9) $ minBound @Int `div` 3 - i
          VGM.write p (12 * i + 10) $ maxBound @Int `div` 3 + i
          VGM.write p (12 * i + 11) $ maxBound @Int `div` 3 - i

        VGM.write p (12 * 11 + 0) 998244353
        VGM.write p (12 * 11 + 1) 1_000_000_007
        VGM.write p (12 * 11 + 2) 1_000_000_009
        VGM.write p (12 * 11 + 3) (-998244353)
        VGM.write p (12 * 11 + 4) (-1_000_000_007)
        VGM.write p (12 * 11 + 5) (-1_000_000_009)
        pure p

  VU.forM_ ps $ \a -> do
    VU.forM_ ps $ \b -> do
      unless (b <= 0) $ do
        let a2 = a `mod` b
        let (!eg1, !eg2) = ACIM.invGcd a b
        let g = gcd a2 b
        g @=? eg1
        assertBool "<=" $ 0 <= eg2
        -- FIXME: not working correctly
        assertBool "<=" $ eg2 <= b `div` eg1
        fromIntegral (g `mod` b) @=? (fromIntegral eg2 :: Word128) * fromIntegral a2 `mod` fromIntegral b

unit_primitiveRootNaive :: TestTree
unit_primitiveRootNaive = testCase "primitiveRootNaive" $ do
  for_ [2 .. 10000] $ \m -> do
    when (ACIM.isPrime m) $ do
      let n = ACIM.primitiveRoot m
      assertBool "<=" $ 1 <= n
      assertBool "<" $ n < m
      x' <-
        foldM
          ( \x _ -> do
              let !xx = x * fromIntegral n `mod` fromIntegral m
              assertBool "/=" $ 1 /= xx
              pure xx
          )
          (1 :: Word64)
          [1 .. m - 2]
      let !x'' = x' * fromIntegral n `mod` fromIntegral m
      1 @=? x''

-- REMARK: too heavy
unit_primitiveRootTemplate :: TestTree
unit_primitiveRootTemplate = testCase "primitiveRootTemplate" $ do
  for_
    [ 2,
      3,
      5,
      7,
      11,
      998244353,
      1000000007,
      469762049,
      167772161,
      754974721,
      324013369,
      831143041,
      1685283601
    ]
    $ \x -> do
      assertBool "" $ isPrimitiveRootNaive x (ACIM.primitiveRoot x)

unit_primitiveRootTest :: TestTree
unit_primitiveRootTest = testCase "primitiveRootTest" $ do
  for_ [0 .. 1000 - 1] $ \i -> do
    let x = fromIntegral $ maxBound @Int32 - i :: Int
    when (ACIM.isPrime x) $ do
      assertBool "" $ isPrimitiveRootNaive x (ACIM.primitiveRoot x)

tests :: [TestTree]
tests =
  [ unit_barrett,
    unit_barrettIntBorder,
    unit_barrettWord32Border,
    unit_isPrime,
    unit_invGcdBound,
    unit_primitiveRootNaive
    -- REMARK: The following primitive root tests take too much time:
    -- unit_primitiveRootTemplate,
    -- unit_primitiveRootTest
  ]
