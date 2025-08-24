module Tests.Extra.AhoCorasick where

import AtCoder.Extra.AhoCorasick qualified as Ac
import Data.Vector qualified as V
import Data.Vector.Algorithms.Intro qualified as VAI
import Data.Vector.Generic qualified as VG
import Data.Vector.Unboxed qualified as VU
import Test.Tasty
import Test.Tasty.QuickCheck as QC

rng :: (Int, Int)
rng = (1, 2)

-- | Generates non-empty string as a vector.
genString :: Gen (VU.Vector Int)
genString = do
  n <- QC.chooseInt rng
  VU.fromList <$> QC.vectorOf n (elements [0 .. 26 - 1])

-- | Generates non-empty strings as a vectors.
-- not empty
genStrings :: Gen (V.Vector (VU.Vector Int))
genStrings = do
  n <- QC.chooseInt rng
  V.fromList <$> QC.vectorOf n genString

prop_empty :: QC.Positive Int -> Bool
prop_empty (QC.Positive n) = do
  -- Not dies
  let !ac0 = Ac.build n V.empty
   in True

prop_numberOfNodes :: TestTree
prop_numberOfNodes = QC.testProperty "numberOfNodes" $ do
  patterns <- genStrings
  let !ac = Ac.build 26 patterns
  pure $ Ac.size ac <= (1 + VG.sum (VG.map VG.length patterns))

tests :: [TestTree]
tests =
  [ QC.testProperty "empty" prop_empty,
    prop_numberOfNodes
  ]
