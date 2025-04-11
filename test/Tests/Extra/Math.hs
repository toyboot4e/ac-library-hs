module Tests.Extra.Math (tests) where

import AtCoder.Extra.Math qualified as ACEM
import Control.Monad (when)
import Data.Foldable (for_)
import Data.Proxy (Proxy (..))
import Data.Semigroup (Max (..), Min (..), Sum (..), mtimesDefault, stimes)
import Data.Vector.Unboxed qualified as VU
import Debug.Trace
import Test.QuickCheck.Property qualified as QC
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck qualified as QC
import Tests.Util (myForAllShrink)

-- orphan instance
instance QC.Arbitrary (Max Int) where
  arbitrary = Max <$> QC.arbitrary

-- orphan instance
instance QC.Arbitrary (Min Int) where
  arbitrary = Min <$> QC.arbitrary

prop_stimes' :: forall a. (Semigroup a, Show a, Eq a, QC.Arbitrary a) => Proxy a -> QC.Property
prop_stimes' _ = myForAllShrink True (const True) desc lhsS lhs rhsS rhs
  where
    desc :: (QC.Positive Int, a) -> [String]
    desc (QC.Positive !n, !s) = ["n = " ++ show n ++ ", s = " ++ show s]
    lhsS = "stimes' n s"
    lhs (QC.Positive !n, !s) = ACEM.stimes' n s
    rhsS = "stimes n s"
    rhs (QC.Positive !n, !s) = stimes n s

prop_mtimes' :: forall a. (Monoid a, Show a, Eq a, QC.Arbitrary a) => Proxy a -> QC.Property
prop_mtimes' _ = myForAllShrink True (const True) desc lhsS lhs rhsS rhs
  where
    desc :: (QC.NonNegative Int, a) -> [String]
    desc (QC.NonNegative !n, !m) = ["n = " ++ show n ++ ", m = " ++ show m]
    lhsS = "mtimes' n s"
    lhs (QC.NonNegative !n, !m) = ACEM.mtimes' n m
    rhsS = "mtimes n s"
    rhs (QC.NonNegative !n, !m) = mtimesDefault n m

-- | This is a solid, fast implementation of prime number enumeration.
truePrimes :: [Int]
truePrimes = 2 : 3 : minus [5, 7 ..] (unionAll [[p * p, p * p + 2 * p ..] | p <- tail truePrimes])
  where
    minus (x : xs) (y : ys) = case compare x y of
      LT -> x : minus xs (y : ys)
      EQ -> minus xs ys
      GT -> minus (x : xs) ys
    minus xs _ = xs

    union (x : xs) (y : ys) = case compare x y of
      LT -> x : union xs (y : ys)
      EQ -> x : union xs ys
      GT -> y : union (x : xs) ys
    union xs [] = xs
    union [] ys = ys

    unionAll :: (Ord a) => [[a]] -> [a]
    unionAll ((x : xs) : t) = x : union xs (unionAll $ pairs t)
      where
        pairs ((x : xs) : ys : t) = (x : union xs ys) : pairs t
        pairs _ = error "unionAll _ pairs: unreachable"
    unionAll _ = error "unionAll: unreachable"

-- unit_primes :: TestTree
-- unit_primes = testCase "primes" $ do
--   for_ [0 .. 10 ^ 9] $ \upper -> do
--     when (upper `mod` 10000 == 0) $ do
--       let !_ = traceShow upper ()
--       pure ()
--     let expected = VU.fromList $ takeWhile (<= upper) truePrimes
--     let result = ACEM.primes upper
--     result @?= expected

tests :: [TestTree]
tests =
  [ testGroup
      "stimes'"
      [ QC.testProperty "Sum" (prop_stimes' (Proxy @(Sum Int))),
        QC.testProperty "Product" (prop_stimes' (Proxy @(Sum Int))),
        QC.testProperty "Max" (prop_stimes' (Proxy @(Max Int))),
        QC.testProperty "Min" (prop_stimes' (Proxy @(Min Int)))
      ],
    testGroup
      "mtimes'"
      [ QC.testProperty "Sum" (prop_mtimes' (Proxy @(Sum Int))),
        QC.testProperty "Product" (prop_mtimes' (Proxy @(Sum Int))),
        QC.testProperty "Max" (prop_mtimes' (Proxy @(Max Int))),
        QC.testProperty "Min" (prop_mtimes' (Proxy @(Min Int)))
      ],
    testGroup
      "primes"
      [ -- unit_primes
      ]
  ]
