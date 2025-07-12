module Tests.TwoSat (tests) where

import AtCoder.TwoSat qualified as TS
import Data.Bit (Bit (..))
import Data.Vector.Unboxed qualified as VU
import Test.Tasty
import Test.Tasty.HUnit

unit_empty :: TestTree
unit_empty = testCase "empty" $ do
  ts <- TS.new 0
  (@?= True) =<< TS.satisfiable ts
  (@?= VU.empty) =<< TS.answer ts

unit_one :: TestTree
unit_one = testCase "one" $ do
  do
    ts <- TS.new 1
    TS.addClause ts 0 True 0 True
    TS.addClause ts 0 False 0 False
    (@?= False) =<< TS.satisfiable ts
  do
    ts <- TS.new 1
    TS.addClause ts 0 True 0 True
    (@?= True) =<< TS.satisfiable ts
    (@?= VU.singleton (Bit True)) =<< TS.answer ts
  do
    ts <- TS.new 1
    TS.addClause ts 0 False 0 False
    (@?= True) =<< TS.satisfiable ts
    (@?= VU.singleton (Bit False)) =<< TS.answer ts

tests :: [TestTree]
tests = [unit_empty, unit_one]
