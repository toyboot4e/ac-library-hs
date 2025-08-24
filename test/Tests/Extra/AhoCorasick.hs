module Tests.Extra.AhoCorasick where

import AtCoder.Extra.AhoCorasick qualified as Ac
import Data.Char (ord)
import Data.List qualified as L
import Data.Vector qualified as V
import Data.Vector.Algorithms qualified as VA
import Data.Vector.Algorithms.Intro qualified as VAI
import Data.Vector.Generic qualified as VG
import Data.Vector.Unboxed qualified as VU
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

rng :: (Int, Int)
rng = (1, 32)

-- | Generates non-empty string as a vector.
genString :: Gen (VU.Vector Int)
genString = do
  VU.fromList <$> QC.listOf1 QC.arbitrary

-- | Generates non-empty strings as a vectors.
-- not empty
genStrings :: Gen (V.Vector (VU.Vector Int))
genStrings = do
  n <- QC.chooseInt rng
  V.fromList <$> QC.vectorOf n genString

prop_empty :: QC.Positive Int -> Bool
prop_empty (QC.Positive n) = do
  -- Not dies
  let !ac0 = Ac.build V.empty
   in True

prop_numberOfNodes :: TestTree
prop_numberOfNodes = QC.testProperty "numberOfNodes" $ do
  patterns <- genStrings
  let !ac = Ac.build patterns
  pure $ Ac.size ac <= (1 + VG.sum (VG.map VG.length patterns))

unit_banana :: TestTree
unit_banana = testCase "banana" $ do
  let patterns =
        V.fromList $
          map
            (VU.map ord . VU.fromList)
            ["banana", "benefit", "banapple", "ban", "fit"]
  let text = VU.map ord $ VU.fromList "banana bans, apple benefits."
  --                        positions:
  --                                   0123456789012345678901234567
  --                                   0         1         2
  --                        5 matches:
  --                                   ban*   ban*        benefit*
  --                                   banana*                fit*

  let ac = Ac.build patterns
  let matches = Ac.match ac text
  matches @?= VU.fromList [(3, 3), (6, 0), (10, 3), (26, 1), (26, 4)]

genAlphabet :: Gen [Int]
genAlphabet = do
  n <- QC.chooseInt (1, 26)
  L.nub <$> QC.vectorOf n QC.arbitrary

genPatternIn :: [Int] -> Gen (VU.Vector Int)
genPatternIn alphabet = do
  n <- QC.chooseInt (1, 6)
  VU.fromList <$> QC.vectorOf n (QC.elements alphabet)

genTextIn :: [Int] -> Gen (VU.Vector Int)
genTextIn alphabet = do
  n <- QC.chooseInt (0, 1000)
  VU.fromList <$> QC.vectorOf n (QC.elements alphabet)

-- | Returns a vector of @(endPos, patternId)@
naiveMatch :: (HasCallStack) => V.Vector (VU.Vector Int) -> VU.Vector Int -> VU.Vector (Int, Int)
naiveMatch patterns text = VU.concatMap f $ VU.generate (VU.length text) (+ 1)
  where
    nPatterns = V.length patterns
    f :: (HasCallStack) => Int -> VU.Vector (Int, Int)
    f endPos = VU.map (endPos,) . VU.filter (test endPos) $ VU.generate nPatterns id
    test endPos iPattern = t == pat
      where
        pat = patterns VG.! iPattern
        len = VU.length pat
        t = VU.drop (endPos - len) $ VU.take endPos text

prop_random :: TestTree
prop_random = QC.testProperty "random" $ do
  alphabet <- genAlphabet
  n <- QC.chooseInt (1, 100)
  -- NOTE: Patterns must be deduplicated in order to get exact result
  patterns <- VA.nub . V.fromList <$> QC.vectorOf n (genPatternIn alphabet)
  let !ac = Ac.build patterns
  text <- genTextIn alphabet
  pure
    . QC.counterexample (show (patterns, "in", text))
    $ VG.modify VAI.sort (Ac.match ac text) QC.=== VG.modify VAI.sort (naiveMatch patterns text)

tests :: [TestTree]
tests =
  [ QC.testProperty "empty" prop_empty,
    prop_numberOfNodes,
    unit_banana,
    prop_random
  ]
