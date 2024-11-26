module Tests.Internal.Queue (tests) where

import AtCoder.Internal.Queue qualified as ACIQ
import Control.Monad.ST (runST)
import Data.Foldable
import Data.Vector.Unboxed qualified as VU
import Test.Tasty
import Test.Tasty.QuickCheck qualified as QC

prop_push :: [Int] -> Bool
prop_push xs =
  let ys = VU.fromList xs
      zs = runST $ do
        buf <- ACIQ.new $ length xs
        for_ xs $ ACIQ.pushBack buf
        ACIQ.unsafeFreeze buf
   in ys == zs

tests :: [TestTree]
tests =
  [ QC.testProperty "push" prop_push
  ]
