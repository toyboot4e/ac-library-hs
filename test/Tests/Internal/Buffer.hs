module Tests.Internal.Buffer (tests) where

import AtCoder.Internal.Buffer qualified as ACB
import Control.Monad
import Control.Monad.ST (runST)
import Data.Foldable
import Data.Maybe
import Data.Vector.Unboxed qualified as VU
import Test.Tasty
import Test.Tasty.QuickCheck as QC

prop_build :: [Int] -> Bool
prop_build xs =
  let ys = VU.fromList xs
      zs = runST $ do
        buf <- ACB.build $ VU.fromList xs
        ACB.unsafeFreeze buf
   in ys == zs

prop_push :: [Int] -> Bool
prop_push xs =
  let ys = VU.fromList xs
      zs = runST $ do
        buf <- ACB.new $ length xs
        for_ xs $ ACB.pushBack buf
        ACB.unsafeFreeze buf
   in ys == zs

prop_pushPop :: [Int] -> Bool
prop_pushPop xs =
  let ys = runST $ do
        buf <- ACB.new $ length xs
        for_ xs $ ACB.pushBack buf
        replicateM (length xs) $ fromJust <$> ACB.popBack buf
   in xs == reverse ys

tests :: [TestTree]
tests =
  [ testGroup
      "AtCoder.Internal.Buffer"
      [ QC.testProperty "build" prop_build,
        QC.testProperty "push" prop_push,
        QC.testProperty "pushPop" prop_pushPop
      ]
  ]
