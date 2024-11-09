module Tests.Internal.MinHeap (tests) where

import AtCoder.Internal.MinHeap qualified as ACIMH
import Control.Monad
import Control.Monad.ST (runST)
import Data.List qualified as L
import Data.Maybe
import Test.Tasty
import Test.Tasty.QuickCheck as QC

prop_ordered :: TestTree
prop_ordered =
  testGroup
    "Ordering"
    [ QC.testProperty "max heap ordering" $ do
        n <- QC.chooseInt (1, 16)
        xs <- QC.vectorOf n (QC.chooseInt (-10, 10))
        let result = runST $ do
              heap <- ACIMH.new n
              forM_ xs (ACIMH.push heap)
              replicateM n (fromJust <$> ACIMH.pop heap)
        let expected = L.sort xs
        return . QC.counterexample (show xs) $ result QC.=== expected
    ]

tests :: [TestTree]
tests = [testGroup "AtCoder.Internal.Heap" [prop_ordered]]
