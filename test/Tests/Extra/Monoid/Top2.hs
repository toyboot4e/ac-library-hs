module Tests.Extra.Monoid.Top2 where

import AtCoder.Extra.Monoid.Top2 qualified as Top2
import Data.Ord (Down (..), comparing)
import Data.Vector.Algorithms.Intro qualified as VAI
import Data.Vector.Unboxed qualified as VU
import Test.Tasty
import Test.Tasty.QuickCheck as QC

naiveTop2 :: VU.Vector Int -> (Int, Int)
naiveTop2 xs = case VU.length xs of
  0 -> (minBound, minBound)
  1 -> (VU.head xs, minBound)
  _ ->
    let xs' = VU.modify (VAI.sortBy (comparing Down)) xs
     in (xs' VU.! 0, xs' VU.! 1)

topGen :: Gen (VU.Vector Int, Top2.Top2 Int)
topGen = do
  x1 <- QC.choose (0, 20)
  x2 <- QC.choose (0, 20)
  let xs = VU.modify (VAI.sortBy (comparing Down)) $ VU.fromList [x1, x2]
  pure $ (xs, Top2.new x1 x2)

insertQueryGen :: Gen (Int, VU.Vector Int)
insertQueryGen = do
  n <- QC.chooseInt (0, 20)
  xs <- VU.fromList <$> QC.vectorOf n (QC.chooseInt (-20, 20))
  pure (n, xs)

prop_new :: TestTree
prop_new = QC.testProperty "new" $ do
  (!xs, !top) <- topGen
  let xs' = VU.modify (VAI.sortBy (comparing Down)) xs
  pure $ (xs' VU.! 0, xs' VU.! 1) QC.=== Top2.unTop2 top

prop_insert :: TestTree
prop_insert = QC.testProperty "insert" $ do
  (!n, !xs) <- insertQueryGen
  pure . QC.conjoin $
    map
      ( \len ->
          let expected = naiveTop2 $ VU.take len xs
              Top2.Top2 result = VU.foldl' (flip Top2.insert) mempty $ VU.take len xs
           in QC.counterexample (show (n, xs)) $
                expected QC.=== result
      )
      [0 .. n - 1]

prop_concat :: TestTree
prop_concat = QC.testProperty "concat" $ do
  (!xs1, !top1) <- topGen
  (!xs2, !top2) <- topGen
  let xs' = VU.modify (VAI.sortBy (comparing Down)) $ xs1 VU.++ xs2
  pure $ (xs' VU.! 0, xs' VU.! 1) QC.=== Top2.unTop2 (top1 <> top2)

tests :: [TestTree]
tests =
  [ prop_new,
    prop_insert,
    prop_concat
  ]
