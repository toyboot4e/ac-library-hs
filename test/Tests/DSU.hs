-- | Disjoint set union tests.
module Tests.DSU (tests) where

import AtCoder.DSU qualified as DSU
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
  uf <- DSU.new 0
  (@?= V.empty) =<< DSU.groups uf

-- empty
-- assign

unit_simple :: TestTree
unit_simple = testCase "simple" $ do
  uf <- DSU.new 2
  (@?= False) =<< DSU.same uf 0 1
  x <- DSU.merge uf 0 1
  (@?= x) =<< DSU.leader uf 0
  (@?= x) =<< DSU.leader uf 1
  (@?= True) =<< DSU.same uf 0 1
  (@?= 2) =<< DSU.size uf 0

unit_line :: TestTree
unit_line = testCase "line" $ do
  let n = 500000
  uf <- DSU.new n
  for_ [0 .. n - 2] $ \i -> do
    DSU.merge uf i (i + 1)
  (@?= n) =<< DSU.size uf 0
  (@?= 1) . V.length =<< DSU.groups uf

unit_lineReverse :: TestTree
unit_lineReverse = testCase "lineReverse" $ do
  let n = 500000
  uf <- DSU.new n
  for_ [n - 2, n - 3 .. 0] $ \i -> do
    DSU.merge uf i (i + 1)
  (@?= n) =<< DSU.size uf 0
  (@?= 1) . V.length =<< DSU.groups uf

spec_invalid :: IO TestTree
spec_invalid = testSpec "invalid" $ do
  it "throws error" $ do
    DSU.new (-1) `shouldThrow` anyException

tests :: [TestTree]
tests =
  [unit_zero, unit_simple, unit_line, unit_lineReverse, unsafePerformIO spec_invalid]
