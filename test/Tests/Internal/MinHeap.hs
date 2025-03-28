module Tests.Internal.MinHeap (tests) where

import AtCoder.Internal.MinHeap qualified as ACIMH
import Control.Monad (replicateM)
import Control.Monad.ST (runST)
import Data.Foldable (for_)
import Data.List qualified as L
import Data.Maybe
import Test.Tasty
import Test.Tasty.QuickCheck qualified as QC

prop_ordered :: TestTree
prop_ordered =
  testGroup
    "Ordering"
    [ QC.testProperty "max heap ordering" $ do
        n <- QC.chooseInt (1, 64)
        xs <- QC.vectorOf n (QC.chooseInt (-64, 64))
        let result = runST $ do
              heap <- ACIMH.new n
              for_ xs (ACIMH.push heap)
              replicateM n (fromJust <$> ACIMH.pop heap)
        let expected = L.sort xs
        pure . QC.counterexample (show xs) $ result QC.=== expected
    ]

tests :: [TestTree]
tests = [testGroup "AtCoder.Internal.Heap" [prop_ordered]]
