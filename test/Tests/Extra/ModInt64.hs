{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}

module Tests.Extra.ModInt64 (tests) where

import AtCoder.Extra.Math qualified as ACEM
import AtCoder.Extra.ModInt64 qualified as M
import AtCoder.ModInt qualified as M32
import Data.Semiring (Ring (..), Semiring (..), WrappedNum (..))
import Data.WideWord.Word128 (Word128 (..))
import Data.Word (Word64 (..))
import GHC.Exts (Proxy#, proxy#)
import GHC.TypeNats (KnownNat, natVal')
import Test.QuickCheck.Classes qualified as QCC
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck qualified as QC
import Tests.Util (laws)

deriving via (WrappedNum Word64) instance Semiring (M.ModInt64 a)

deriving via (WrappedNum Word64) instance Ring (M.ModInt64 a)

type M1 = 3

type M2 = 5

type M3 = 998244353

type M4 = 1000000007

type M5 = 4611686018427387847

instance M32.Modulus M1 where
  {-# INLINE isPrimeModulus #-}
  isPrimeModulus _ = True

  -- FIXME: wrong
  primitiveRootModulus _ = (-1)

instance M32.Modulus M2 where
  {-# INLINE isPrimeModulus #-}
  isPrimeModulus _ = True

  -- FIXME: wrong
  primitiveRootModulus _ = (-1)

instance (KnownNat a) => QC.Arbitrary (M.ModInt64 a) where
  arbitrary = M.new <$> QC.arbitrary

to128 :: (Integral a) => a -> Word128
to128 = fromIntegral

mulMod :: Int -> Int -> Int -> Int
mulMod m x y = fromIntegral . word128Lo64 $! (to128 (x `mod` m) * to128 (y `mod` m)) `mod` to128 m

unit_literal :: forall a. (KnownNat a) => Proxy# a -> TestTree
unit_literal proxy = testCase "literal" $ do
  let !m :: Int = fromIntegral $ natVal' proxy
  (@?= (0 `mod` m)) $ fromIntegral (0 :: M.ModInt64 a)
  (@?= ((-1) `mod` m)) $ fromIntegral (-1 :: M.ModInt64 a)
  (@?= (1 `mod` m)) $ fromIntegral (1 :: M.ModInt64 a)
  (@?= (m `mod` m)) $ fromIntegral (M.new @a m)
  (@?= ((m - 1) `mod` m)) $ fromIntegral (M.new @a (m - 1))
  (@?= ((m + 1) `mod` m)) $ fromIntegral (M.new @a (m + 1))

prop_addMod :: forall a. (KnownNat a) => Proxy# a -> Int -> Int -> QC.Property
prop_addMod proxy x y =
  let !m = fromIntegral $ natVal' proxy
      !res = M.val $ M.new @a x + M.new @a y
      !expected = (x + y) `mod` m
   in res QC.=== expected

prop_subMod :: forall a. (KnownNat a) => Proxy# a -> Int -> Int -> QC.Property
prop_subMod proxy x y =
  let !m = fromIntegral $ natVal' proxy
      !res = M.val $ M.new @a x - M.new @a y
      !expected = (x - y) `mod` m
   in res QC.=== expected

prop_mulMod :: forall a. (KnownNat a) => Proxy# a -> Int -> Int -> QC.Property
prop_mulMod proxy x y =
  let !m = fromIntegral $ natVal' proxy
      !res = M.val $ M.new @a x * M.new @a y
      !expected = mulMod m x y
   in res QC.=== expected

prop_powMod :: forall a. (KnownNat a) => Proxy# a -> Int -> QC.Positive Int -> QC.Property
prop_powMod proxy x (QC.Positive n) =
  let !m = fromIntegral $ natVal' proxy
      !res = M.val $ M.pow (M.new @a x) n
      !expected = ACEM.power (mulMod m) n (x `mod` m)
   in res QC.=== expected

prop_inv :: forall a. (KnownNat a) => M.ModInt64 a -> QC.Property
prop_inv x =
  M.val x
    /= 0
    QC.==> QC.counterexample (show x)
    $ QC.conjoin
      [ M.inv x * x QC.=== M.new 1,
        M.new 1 QC.=== M.inv x * x
      ]

prop_quotRem :: forall a. (M32.Modulus a) => Proxy# a -> Int -> QC.NonZero Int -> QC.Property
prop_quotRem _ x (QC.NonZero y) =
  (y `mod` m /= 0) QC.==>
    let (!resQ, !resR) = M.new @a x `quotRem` M.new @a y
        (!expQ, !expR) = M32.new @a x `quotRem` M32.new @a y
     in QC.conjoin
          [ M.val resQ QC.=== M32.val expQ,
            M.val resR QC.=== M32.val expR
          ]
  where
    !m = fromIntegral $ natVal' (proxy# @a)

prop_quotRem2 :: forall a. (KnownNat a) => Proxy# a -> Int -> QC.NonZero Int -> QC.Property
prop_quotRem2 _ x (QC.NonZero y) =
  let !a = M.new @a x
      !d = M.new @a y
   in d /= 0 QC.==>
        let (!q, !r) = a `quotRem` d
         in (d * q + r) QC.=== a

prop_eq :: forall a. (KnownNat a) => Proxy# a -> Word64 -> Word64 -> QC.Property
prop_eq _ x y = lhs QC.=== rhs
  where
    !lhs = M.new64 @a x == M.new64 @a y
    !m = fromIntegral $ natVal' (proxy# @a)
    !rhs = x `mod` m == y `mod` m

-- Cannot create list for unlifted types
--
-- {-# LANGUAGE ImpredicativeTypes #-}
-- modProps :: String -> [forall a. (KnownNat a) => Proxy# a -> QC.Property] -> TestTree
-- modProps title prop =
--   testGroup title $
--     map
--       (\proxy -> QC.testProperty (show (natVal' proxy)) (prop proxy))
--       [proxy# @M1, proxy# @M2, proxy# @M3, proxy# @M4, proxy# @M5]

tests :: [TestTree]
tests =
  [ testGroup
      "literal"
      [ unit_literal (proxy# @M1),
        unit_literal (proxy# @M2),
        unit_literal (proxy# @M3),
        unit_literal (proxy# @M4),
        unit_literal (proxy# @M5)
      ],
    testGroup
      "inv"
      [ QC.testProperty "1" (prop_inv @M1),
        QC.testProperty "2" (prop_inv @M2),
        QC.testProperty "3" (prop_inv @M3),
        QC.testProperty "4" (prop_inv @M4),
        QC.testProperty "5" (prop_inv @M5)
      ],
    testGroup
      "quotRem"
      [ QC.testProperty "1" (prop_quotRem (proxy# @M1)),
        QC.testProperty "2" (prop_quotRem (proxy# @M2)),
        QC.testProperty "3" (prop_quotRem (proxy# @M3)),
        QC.testProperty "4" (prop_quotRem (proxy# @M4))
        -- 64 bit
        -- QC.testProperty "5" (prop_quotRem (proxy# @M5))
      ],
    testGroup
      "quotRem2"
      [ QC.testProperty "1" (prop_quotRem2 (proxy# @M1)),
        QC.testProperty "2" (prop_quotRem2 (proxy# @M2)),
        QC.testProperty "3" (prop_quotRem2 (proxy# @M3)),
        QC.testProperty "4" (prop_quotRem2 (proxy# @M4)),
        QC.testProperty "5" (prop_quotRem2 (proxy# @M5))
      ],
    testGroup
      "eq"
      [ QC.testProperty "1" (prop_eq (proxy# @M1)),
        QC.testProperty "2" (prop_eq (proxy# @M2)),
        QC.testProperty "3" (prop_eq (proxy# @M3)),
        QC.testProperty "4" (prop_eq (proxy# @M4)),
        QC.testProperty "5" (prop_eq (proxy# @M5))
      ],
    testGroup
      "addMod"
      [ QC.testProperty "1" (prop_addMod (proxy# @M1)),
        QC.testProperty "2" (prop_addMod (proxy# @M2)),
        QC.testProperty "3" (prop_addMod (proxy# @M3)),
        QC.testProperty "4" (prop_addMod (proxy# @M4)),
        QC.testProperty "5" (prop_addMod (proxy# @M5))
      ],
    testGroup
      "subMod"
      [ QC.testProperty "1" (prop_subMod (proxy# @M1)),
        QC.testProperty "2" (prop_subMod (proxy# @M2)),
        QC.testProperty "3" (prop_subMod (proxy# @M3)),
        QC.testProperty "4" (prop_subMod (proxy# @M4)),
        QC.testProperty "5" (prop_subMod (proxy# @M5))
      ],
    testGroup
      "mulMod"
      [ QC.testProperty "1" (prop_mulMod (proxy# @M1)),
        QC.testProperty "2" (prop_mulMod (proxy# @M2)),
        QC.testProperty "3" (prop_mulMod (proxy# @M3)),
        QC.testProperty "4" (prop_mulMod (proxy# @M4)),
        QC.testProperty "5" (prop_mulMod (proxy# @M5))
      ],
    testGroup
      "powMod"
      [ QC.testProperty "1" (prop_powMod (proxy# @M1)),
        QC.testProperty "2" (prop_powMod (proxy# @M2)),
        QC.testProperty "3" (prop_powMod (proxy# @M3)),
        QC.testProperty "4" (prop_powMod (proxy# @M4)),
        QC.testProperty "5" (prop_powMod (proxy# @M5))
      ],
    testGroup
      "laws"
      [ laws @(M.ModInt64 M5)
          [ QCC.eqLaws,
            QCC.numLaws,
            QCC.integralLaws,
            QCC.ordLaws,
            QCC.enumLaws,
            QCC.boundedEnumLaws,
            QCC.primLaws,
            QCC.semiringLaws,
            QCC.ringLaws,
            QCC.showReadLaws
          ]
      ]
  ]
