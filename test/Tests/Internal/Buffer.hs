module Tests.Internal.Buffer (tests) where

import AtCoder.Internal.Buffer qualified as ACB
import Control.Monad.ST (runST)
import Data.Foldable
import Data.Vector.Unboxed qualified as VU
import Test.Tasty
import Test.Tasty.QuickCheck as QC

push :: [Int] -> Bool
push xs =
  let ys = VU.fromList xs
      zs = runST $ do
        buf <- ACB.new $ length xs
        for_ xs $ ACB.pushBack buf
        ACB.unsafeFreeze buf
   in ys == zs

tests :: [TestTree]
tests =
  [ testGroup
      "AtCoder.Internal.Buffer"
      [ QC.testProperty "push" push
      ]
  ]