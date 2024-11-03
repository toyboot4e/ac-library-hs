-- | Disjoint set union tests.
module Tests.Dsu (tests) where

import AtCoder.Dsu qualified as Dsu
import AtCoder.FenwickTree qualified as FT
import Data.Foldable
import Data.Vector qualified as V
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hspec

unit_zero :: TestTree
unit_zero = testCase "zero" $ do
  uf <- Dsu.new 0
  (@?= V.empty) =<< Dsu.groups uf

-- empty
-- assign

unit_simple :: TestTree
unit_simple = testCase "simple" $ do
  uf <- Dsu.new 2
  (@?= False) =<< Dsu.same uf 0 1
  x <- Dsu.merge uf 0 1
  (@?= x) =<< Dsu.leader uf 0
  (@?= x) =<< Dsu.leader uf 1
  (@?= True) =<< Dsu.same uf 0 1
  (@?= 2) =<< Dsu.size uf 0

unit_line :: TestTree
unit_line = testCase "line" $ do
  let n = 500000
  uf <- Dsu.new n
  for_ [0 .. n - 2] $ \i -> do
    Dsu.merge uf i (i + 1)
  (@?= n) =<< Dsu.size uf 0
  (@?= 1) . V.length =<< Dsu.groups uf

unit_lineReverse :: TestTree
unit_lineReverse = testCase "lineReverse" $ do
  let n = 500000
  uf <- Dsu.new n
  for_ [n - 2, n - 3 .. 0] $ \i -> do
    Dsu.merge uf i (i + 1)
  (@?= n) =<< Dsu.size uf 0
  (@?= 1) . V.length =<< Dsu.groups uf

spec_invalid :: IO TestTree
spec_invalid = testSpec "invalid" $ do
  it "throws error" $ do
    Dsu.new (-1) `shouldThrow` anyException

tests :: [TestTree]
tests =
  [unit_zero, unit_simple, unit_line, unit_lineReverse, unsafePerformIO spec_invalid]
