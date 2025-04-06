module Tests.Extra.Ix0 where

import AtCoder.Extra.Ix0
import Test.Tasty
import Test.Tasty.QuickCheck as QC

prop_ix1 :: QC.Gen QC.Property
prop_ix1 = do
  d1 <- QC.chooseInt (1, 10)
  let expected = [0 .. d1 - 1]
  let res = [index0 d1 x1 | x1 <- [0 .. d1 - 1]]
  pure $ res QC.=== expected

prop_ix2 :: QC.Gen QC.Property
prop_ix2 = do
  d2 <- QC.chooseInt (1, 10)
  d1 <- QC.chooseInt (1, 10)
  let expected = [0 .. d2 * d1 - 1]
  let res = [index0 (d2, d1) (x2, x1) | x2 <- [0 .. d2 - 1], x1 <- [0 .. d1 - 1]]
  pure $ res QC.=== expected

prop_ix3 :: QC.Gen QC.Property
prop_ix3 = do
  d3 <- QC.chooseInt (1, 10)
  d2 <- QC.chooseInt (1, 10)
  d1 <- QC.chooseInt (1, 10)
  let expected = [0 .. d3 * d2 * d1 - 1]
  let res = [index0 (d3, d2, d1) (x3, x2, x1) | x3 <- [0 .. d3 - 1], x2 <- [0 .. d2 - 1], x1 <- [0 .. d1 - 1]]
  pure $ res QC.=== expected

prop_ix4 :: QC.Gen QC.Property
prop_ix4 = do
  d4 <- QC.chooseInt (1, 10)
  d3 <- QC.chooseInt (1, 10)
  d2 <- QC.chooseInt (1, 10)
  d1 <- QC.chooseInt (1, 10)
  let expected = [0 .. d4 * d3 * d2 * d1 - 1]
  let res = [index0 (d4, d3, d2, d1) (x4, x3, x2, x1) | x4 <- [0 .. d4 - 1], x3 <- [0 .. d3 - 1], x2 <- [0 .. d2 - 1], x1 <- [0 .. d1 - 1]]
  pure $ res QC.=== expected

prop_ix5 :: QC.Gen QC.Property
prop_ix5 = do
  d5 <- QC.chooseInt (1, 10)
  d4 <- QC.chooseInt (1, 10)
  d3 <- QC.chooseInt (1, 10)
  d2 <- QC.chooseInt (1, 10)
  d1 <- QC.chooseInt (1, 10)
  let expected = [0 .. d5 * d4 * d3 * d2 * d1 - 1]
  let res = [index0 (d5, d4, d3, d2, d1) (x5, x4, x3, x2, x1) | x5 <- [0 .. d5 - 1], x4 <- [0 .. d4 - 1], x3 <- [0 .. d3 - 1], x2 <- [0 .. d2 - 1], x1 <- [0 .. d1 - 1]]
  pure $ res QC.=== expected

prop_ix6 :: QC.Gen QC.Property
prop_ix6 = do
  d6 <- QC.chooseInt (1, 10)
  d5 <- QC.chooseInt (1, 10)
  d4 <- QC.chooseInt (1, 10)
  d3 <- QC.chooseInt (1, 10)
  d2 <- QC.chooseInt (1, 10)
  d1 <- QC.chooseInt (1, 10)
  let expected = [0 .. d6 * d5 * d4 * d3 * d2 * d1 - 1]
  let res = [index0 (d6, d5, d4, d3, d2, d1) (x6, x5, x4, x3, x2, x1) | x6 <- [0 .. d6 - 1], x5 <- [0 .. d5 - 1], x4 <- [0 .. d4 - 1], x3 <- [0 .. d3 - 1], x2 <- [0 .. d2 - 1], x1 <- [0 .. d1 - 1]]
  pure $ res QC.=== expected

-- indices should be successive
tests :: [TestTree]
tests =
  [ QC.testProperty "Ix0 1" prop_ix1,
    QC.testProperty "Ix0 2" prop_ix2,
    QC.testProperty "Ix0 3" prop_ix3,
    QC.testProperty "Ix0 4" prop_ix4,
    QC.testProperty "Ix0 5" prop_ix5,
    QC.testProperty "Ix0 6" prop_ix6
  ]
