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

zero :: TestTree
zero = testCase "zero" $ do
  uf <- DSU.new 0
  (@?= V.empty) =<< DSU.groups uf

-- empty
-- assign

simple :: TestTree
simple = testCase "simple" $ do
  uf <- DSU.new 2
  (@?= False) =<< DSU.same uf 0 1
  x <- DSU.merge uf 0 1
  (@?= x) =<< DSU.leader uf 0
  (@?= x) =<< DSU.leader uf 1
  (@?= True) =<< DSU.same uf 0 1
  (@?= 2) =<< DSU.size uf 0

line :: TestTree
line = testCase "line" $ do
  let n = 500000
  uf <- DSU.new n
  for_ [0 .. n - 2] $ \i -> do
    DSU.merge uf i (i + 1)
  (@?= n) =<< DSU.size uf 0
  (@?= 1) . V.length =<< DSU.groups uf

lineReverse :: TestTree
lineReverse = testCase "lineReverse" $ do
  let n = 500000
  uf <- DSU.new n
  for_ [n - 2, n - 3 .. 0] $ \i -> do
    DSU.merge uf i (i + 1)
  (@?= n) =<< DSU.size uf 0
  (@?= 1) . V.length =<< DSU.groups uf

invalid :: IO TestTree
invalid = testSpec "invalid" $ do
  it "throws error" $ do
    DSU.new (-1) `shouldThrow` anyException

tests :: [TestTree]
tests =
  [zero, simple, line, lineReverse, unsafePerformIO invalid]
