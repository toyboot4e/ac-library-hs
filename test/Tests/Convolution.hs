{-# LANGUAGE DataKinds #-}

module Tests.Convolution (tests) where

import AtCoder.Convolution qualified as ACC
import AtCoder.Internal.Convolution qualified as ACIC
import AtCoder.ModInt qualified as AM
import Data.Proxy (Proxy (..))
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import Data.Word (Word64)
import Test.QuickCheck qualified as QC
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck qualified as QC

-- | Orphan instance..
instance AM.Modulus 924844033 where
  isPrimeModulus _ = True
  primitiveRootModulus _ = 5

convModNaive :: Int -> VU.Vector Int -> VU.Vector Int -> VU.Vector Int
convModNaive modulus a b = VU.create $ do
  c <- VUM.replicate (VU.length a + VU.length b - 1) 0
  VU.iforM_ a $ \i ai -> do
    VU.iforM_ b $ \j bj -> do
      VGM.modify c ((`mod` modulus) . (+ ai * bj)) (i + j)
  pure c

convMintNaive :: (AM.Modulus p) => VU.Vector (AM.ModInt p) -> VU.Vector (AM.ModInt p) -> VU.Vector (AM.ModInt p)
convMintNaive a b = VU.create $ do
  c <- VUM.replicate (VU.length a + VU.length b - 1) 0
  VU.iforM_ a $ \i ai -> do
    VU.iforM_ b $ \j bj -> do
      VGM.modify c (+ ai * bj) (i + j)
  pure c

conv64Naive :: VU.Vector Int -> VU.Vector Int -> VU.Vector Int
conv64Naive a b = VU.create $ do
  c <- VUM.replicate (VU.length a + VU.length b - 1) 0
  VU.iforM_ a $ \i ai -> do
    VU.iforM_ b $ \j bj -> do
      VGM.modify c (+ ai * bj) (i + j)
  pure c

unit_empty :: TestTree
unit_empty = testCase "empty" $ do
  -- any `Integral a` is allowed
  VU.empty @=? ACC.convolutionMod (Proxy @998244353) (VU.empty @Int) (VU.empty @Int)
  VU.empty @=? ACC.convolutionMod (Proxy @998244353) (VU.empty @Int) (VU.fromList @Int [1, 2])
  VU.empty @=? ACC.convolutionMod (Proxy @998244353) (VU.fromList @Int [1, 2]) (VU.empty @Int)
  VU.empty @=? ACC.convolutionMod (Proxy @998244353) (VU.fromList @Int [1]) (VU.empty @Int)

  VU.empty @=? ACC.convolutionMod (Proxy @998244353) (VU.empty @Word64) (VU.empty @Word64)
  VU.empty @=? ACC.convolutionMod (Proxy @998244353) (VU.empty @Word64) (VU.fromList @Word64 [1, 2])
  VU.empty @=? ACC.convolutionMod (Proxy @998244353) (VU.fromList @Word64 [1, 2]) (VU.empty @Word64)
  VU.empty @=? ACC.convolutionMod (Proxy @998244353) (VU.fromList @Word64 [1]) (VU.empty @Word64)

  VU.empty @=? ACC.convolutionMod (Proxy @998244353) (VU.empty @(AM.ModInt 998244353)) (VU.empty @(AM.ModInt 998244353))
  VU.empty @=? ACC.convolutionMod (Proxy @998244353) (VU.empty @(AM.ModInt 998244353)) (VU.fromList @(AM.ModInt 998244353) [1, 2])
  VU.empty @=? ACC.convolutionMod (Proxy @998244353) (VU.fromList @(AM.ModInt 998244353) [1, 2]) (VU.empty @(AM.ModInt 998244353))
  VU.empty @=? ACC.convolutionMod (Proxy @998244353) (VU.fromList @(AM.ModInt 998244353) [1]) (VU.empty @(AM.ModInt 998244353))

-- FIXME: the naive calculation seems like too slow
prop_mid :: TestTree
prop_mid = QC.testProperty "mid" $ do
  -- any `Integral a` is allowed
  let modInt :: Int -> AM.ModInt998244353
      modInt = AM.new
  a <- VU.map modInt . VU.fromList <$> QC.vectorOf 1234 (QC.arbitrary @Int)
  b <- VU.map modInt . VU.fromList <$> QC.vectorOf 2345 (QC.arbitrary @Int)
  pure $ convMintNaive a b QC.=== ACC.convolution a b

unit_butterfly :: TestTree
unit_butterfly = testCase "butterfly" $ do
  let modInt :: Int -> AM.ModInt998244353
      modInt = AM.new
  let expected = VU.fromList [10, 998244351, 173167434, 825076915]
  vec <- VU.unsafeThaw $ VU.map modInt $ VU.fromList [1, 2, 3, 4]
  ACIC.butterfly vec
  (expected @=?) =<< VU.unsafeFreeze vec

unit_invButterfly :: TestTree
unit_invButterfly = testCase "invButterfly" $ do
  let modInt :: Int -> AM.ModInt998244353
      modInt = AM.new
  let expected = VU.fromList [10, 911660634, 998244349, 86583717]
  vec <- VU.unsafeThaw $ VU.map modInt $ VU.fromList [1, 2, 3, 4]
  ACIC.butterflyInv vec
  (expected @=?) =<< VU.unsafeFreeze vec

testWithRangeMint ::
  forall p.
  (AM.Modulus p) =>
  (VU.Vector (AM.ModInt p) -> VU.Vector (AM.ModInt p) -> QC.Property) ->
  QC.Gen QC.Property
testWithRangeMint f = do
  m <- QC.chooseInt (1, 20 - 1)
  n <- QC.chooseInt (1, 20 - 1)
  a <- VU.map (AM.new @p) . VU.fromList <$> QC.vectorOf n (QC.arbitrary @Int)
  b <- VU.map (AM.new @p) . VU.fromList <$> QC.vectorOf m (QC.arbitrary @Int)
  pure $ f a b

testWithRangeInt :: forall p. (AM.Modulus p) => Proxy p -> (VU.Vector Int -> VU.Vector Int -> QC.Property) -> QC.Gen QC.Property
testWithRangeInt _ f = do
  m <- QC.chooseInt (1, 20 - 1)
  n <- QC.chooseInt (1, 20 - 1)
  let !modulus = AM.modVal (Proxy @p)
  a <- VU.map (`mod` modulus) . VU.fromList <$> QC.vectorOf n (QC.arbitrary @Int)
  b <- VU.map (`mod` modulus) . VU.fromList <$> QC.vectorOf m (QC.arbitrary @Int)
  pure $ f a b

prop_simpleSMod1 :: TestTree
prop_simpleSMod1 = QC.testProperty "simpleSMod1"
  . testWithRangeMint @998244353
  $ \a b -> convMintNaive a b QC.=== ACC.convolution a b

prop_simpleSMod2 :: TestTree
prop_simpleSMod2 = QC.testProperty "simpleSMod1"
  . testWithRangeMint @924844033
  $ \a b -> convMintNaive a b QC.=== ACC.convolution a b

prop_simpleInt0 :: TestTree
prop_simpleInt0 = QC.testProperty "simpleInt0"
  . testWithRangeInt (Proxy @998244353)
  $ \a b -> convModNaive modulus a b QC.=== VU.map AM.val (ACC.convolution (VU.map modInt a) (VU.map modInt b))
  where
    modInt = AM.new @998244353
    !modulus = AM.modVal (Proxy @998244353)

prop_simpleInt1 :: TestTree
prop_simpleInt1 = QC.testProperty "simpleInt1"
  . testWithRangeInt (Proxy @998244353)
  $ \a b -> convModNaive modulus a b QC.=== ACC.convolutionMod (Proxy @998244353) a b
  where
    !modulus = AM.modVal (Proxy @998244353)

prop_simpleInt2 :: TestTree
prop_simpleInt2 = QC.testProperty "simpleInt2"
  . testWithRangeInt (Proxy @924844033)
  $ \a b -> convModNaive modulus a b QC.=== ACC.convolutionMod (Proxy @924844033) a b
  where
    !modulus = AM.modVal (Proxy @924844033)

testWithRange64 :: (VU.Vector Int -> VU.Vector Int -> QC.Property) -> QC.Gen QC.Property
testWithRange64 f = do
  m <- QC.chooseInt (1, 20 - 1)
  n <- QC.chooseInt (1, 20 - 1)
  a <- VU.map (fromIntegral . subtract 500_000 . (`mod` 1_000_000)) . VU.fromList <$> QC.vectorOf n (QC.arbitrary @Word64)
  b <- VU.map (fromIntegral . subtract 500_000 . (`mod` 1_000_000)) . VU.fromList <$> QC.vectorOf m (QC.arbitrary @Word64)
  pure $ f a b

prop_conv64 :: TestTree
prop_conv64 = QC.testProperty "conv64"
  . testWithRange64
  $ \a b -> conv64Naive a b QC.=== ACC.convolution64 a b

-- TODO: Shall we use Word64? (#41)
prop_conv64Bound :: [TestTree]
prop_conv64Bound =
  [ QC.testProperty "conv64Bound_1" $ do
      i <- fromIntegral <$> QC.chooseInt (-1000, 1000)
      let a = VU.singleton (minBound @Int - m1m2 - m1m3 - m2m3 + i)
      let b = VU.singleton 1
      pure $ a QC.=== ACC.convolution64 a b,
    QC.testProperty "conv64Bound_2" $ do
      i <- fromIntegral <$> QC.chooseInt (0, 1000 - 1)
      let a = VU.singleton $ (0 :: Int) + i
      let b = VU.singleton 1
      pure $ a QC.=== ACC.convolution64 a b,
    QC.testProperty "conv64Bound_3" $ do
      i <- fromIntegral <$> QC.chooseInt (0, 1000 - 1)
      let a = VU.singleton $ maxBound @Int + i
      let b = VU.singleton 1
      pure $ a QC.=== ACC.convolution64 a b
  ]
  where
    mod1 :: Int = 469762049
    mod2 :: Int = 167772161
    mod3 :: Int = 754974721
    m2m3 :: Int = mod2 * mod3
    m1m3 :: Int = mod1 * mod3
    m1m2 :: Int = mod1 * mod2

instance AM.Modulus 641 where
  isPrimeModulus _ = True
  primitiveRootModulus _ = 3

prop_conv641 :: TestTree
prop_conv641 = QC.testProperty "prop_conv641" $ do
  let modulus = 641
  a <- VU.fromList <$> QC.vectorOf 64 (QC.chooseInt (0, modulus - 1))
  b <- VU.fromList <$> QC.vectorOf 65 (QC.chooseInt (0, modulus - 1))
  pure $ convModNaive modulus a b QC.=== ACC.convolutionMod (Proxy @641) a b

instance AM.Modulus 18433 where
  isPrimeModulus _ = True
  primitiveRootModulus _ = 5

prop_conv18433 :: TestTree
prop_conv18433 = QC.testProperty "prop_conv18433" $ do
  let modulus = 18433
  a <- VU.fromList <$> QC.vectorOf 1024 (QC.chooseInt (0, modulus - 1))
  b <- VU.fromList <$> QC.vectorOf 1025 (QC.chooseInt (0, modulus - 1))
  pure $ convModNaive modulus a b QC.=== ACC.convolutionMod (Proxy @18433) a b

unit_conv2 :: TestTree
unit_conv2 = testCase "prop_conv2" $ do
  VU.empty @Int @=? ACC.convolutionMod (Proxy @2) VU.empty VU.empty

instance AM.Modulus 257 where
  isPrimeModulus _ = True
  primitiveRootModulus _ = 3

prop_conv257 :: TestTree
prop_conv257 = QC.testProperty "prop_conv257" $ do
  let modulus = 257
  a <- VU.fromList <$> QC.vectorOf 128 (QC.chooseInt (0, modulus - 1))
  b <- VU.fromList <$> QC.vectorOf 129 (QC.chooseInt (0, modulus - 1))
  pure $ convModNaive modulus a b QC.=== ACC.convolutionMod (Proxy @257) a b

instance AM.Modulus 2147483647 where
  isPrimeModulus _ = True
  primitiveRootModulus _ = 7

prop_conv2147483647 :: TestTree
prop_conv2147483647 = QC.testProperty "prop_conv2147483647" $ do
  a <- VU.map (AM.new @2147483647) . VU.fromList <$> QC.vectorOf 1 (QC.arbitrary @Int)
  b <- VU.map (AM.new @2147483647) . VU.fromList <$> QC.vectorOf 2 (QC.arbitrary @Int)
  pure $ convMintNaive a b QC.=== ACC.convolution a b

instance AM.Modulus 2130706433 where
  isPrimeModulus _ = True
  primitiveRootModulus _ = 3

prop_conv2130706433 :: TestTree
prop_conv2130706433 = QC.testProperty "prop_conv2130706433" $ do
  a <- VU.map (AM.new @2130706433) . VU.fromList <$> QC.vectorOf 1024 (QC.arbitrary @Int)
  b <- VU.map (AM.new @2130706433) . VU.fromList <$> QC.vectorOf 1024 (QC.arbitrary @Int)
  pure $ convMintNaive a b QC.=== ACC.convolution a b

unit_bigPrime :: TestTree
unit_bigPrime = testCase "bigPrime" $ do
  let a = VU.map (AM.new @2130706433) $ VU.fromList [1, 2130706432, 1, 0, 2130706432]
  let b = VU.map (AM.new @2130706433) $ VU.fromList [1, 1, 2130706432, 0, 0]
  convMintNaive a b @=? ACIC.convolutionFft a b

tests :: [TestTree]
tests =
  [ unit_empty,
    -- prop_mid, -- slow
    unit_butterfly,
    unit_invButterfly,
    unit_bigPrime,
    prop_simpleSMod1,
    prop_simpleSMod2,
    prop_simpleInt0,
    prop_simpleInt1,
    prop_simpleInt2,
    prop_conv64,
    prop_conv641,
    prop_conv18433,
    unit_conv2,
    prop_conv257,
    prop_conv2147483647,
    prop_conv2130706433
  ]
    ++ prop_conv64Bound
