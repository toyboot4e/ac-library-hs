module Tests.Extra.Monoid.Top4 where

import AtCoder.Extra.Monoid.Top4 qualified as Top4
import Data.Ord (Down (..), comparing)
import Data.Vector.Algorithms.Intro qualified as VAI
import Data.Vector.Unboxed qualified as VU
import Test.Tasty
import Test.Tasty.QuickCheck as QC

naiveTop4 :: VU.Vector Int -> (Int, Int, Int, Int)
naiveTop4 xs = case VU.length xs of
  0 -> (minBound, minBound, minBound, minBound)
  1 -> (xs' VU.! 0, minBound, minBound, minBound)
  2 -> (xs' VU.! 0, xs' VU.! 1, minBound, minBound)
  3 -> (xs' VU.! 0, xs' VU.! 1, xs' VU.! 2, minBound)
  _ -> (xs' VU.! 0, xs' VU.! 1, xs' VU.! 2, xs' VU.! 3)
  where
    xs' = VU.modify (VAI.sortBy (comparing Down)) xs

topGen :: Gen (VU.Vector Int, Top4.Top4 Int)
topGen = do
  x1 <- QC.choose (0, 20)
  x2 <- QC.choose (0, 20)
  x3 <- QC.choose (0, 20)
  x4 <- QC.choose (0, 20)
  let xs = VU.modify (VAI.sortBy (comparing Down)) $ VU.fromList [x1, x2, x3, x4]
  pure $ (xs, Top4.new x1 x2 x3 x4)

insertQueryGen :: Gen (Int, VU.Vector Int)
insertQueryGen = do
  n <- QC.chooseInt (0, 20)
  xs <- VU.fromList <$> QC.vectorOf n (QC.chooseInt (-20, 20))
  pure (n, xs)

prop_new :: TestTree
prop_new = QC.testProperty "new" $ do
  (!xs, !top) <- topGen
  let xs' = VU.modify (VAI.sortBy (comparing Down)) xs
  pure $ (xs' VU.! 0, xs' VU.! 1, xs' VU.! 2, xs' VU.! 3) QC.=== Top4.unTop4 top

prop_insert :: TestTree
prop_insert = QC.testProperty "insert" $ do
  (!n, !xs) <- insertQueryGen
  pure . QC.conjoin $
    map
      ( \len ->
          let expected = naiveTop4 $ VU.take len xs
              Top4.Top4 result = VU.foldl' (flip Top4.insert) mempty $ VU.take len xs
           in QC.counterexample (show (n, xs)) $
                expected QC.=== result
      )
      [0 .. n - 1]

prop_concat :: TestTree
prop_concat = QC.testProperty "concat" $ do
  (!xs1, !top1) <- topGen
  (!xs2, !top2) <- topGen
  let xs' = VU.modify (VAI.sortBy (comparing Down)) $ xs1 VU.++ xs2
  pure $ (xs' VU.! 0, xs' VU.! 1, xs' VU.! 2, xs' VU.! 3) QC.=== Top4.unTop4 (top1 <> top2)

tests :: [TestTree]
tests =
  [ prop_new,
    prop_insert,
    prop_concat
  ]
