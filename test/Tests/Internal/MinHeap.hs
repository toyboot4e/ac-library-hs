module Tests.Internal.MinHeap (tests) where

import AtCoder.Internal.MinHeap qualified as ACMH
import Control.Monad
import Control.Monad.ST (runST)
import Data.List qualified as L
import Data.Maybe
import Test.Tasty
import Test.Tasty.QuickCheck as QC

ordered :: TestTree
ordered =
  testGroup
    "Ordering"
    [ QC.testProperty "max heap ordering" $ do
        n <- QC.chooseInt (1, 16)
        xs <- QC.vectorOf n (QC.chooseInt (-10, 10))
        let result = runST $ do
              heap <- ACMH.new n
              forM_ xs (ACMH.push heap)
              replicateM n (fromJust <$> ACMH.pop heap)
        let expected = L.sort xs
        return . QC.counterexample (show xs) $ result QC.=== expected
    ]

tests :: [TestTree]
tests = [testGroup "AtCoder.Internal.Heap" [ordered]]
