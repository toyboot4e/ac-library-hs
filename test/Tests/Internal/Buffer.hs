module Tests.Internal.Buffer (tests) where

import AtCoder.Internal.Buffer qualified as ACIB
import Control.Monad
import Control.Monad.ST (runST)
import Data.Foldable
import Data.Maybe
import Data.Vector.Unboxed qualified as VU
import Test.Tasty
import Test.Tasty.QuickCheck qualified as QC

prop_build :: [Int] -> Bool
prop_build xs =
  let ys = VU.fromList xs
      zs = runST $ do
        buf <- ACIB.build $ VU.fromList xs
        ACIB.unsafeFreeze buf
   in ys == zs

prop_push :: [Int] -> Bool
prop_push xs =
  let ys = VU.fromList xs
      zs = runST $ do
        buf <- ACIB.new $ length xs
        for_ xs $ ACIB.pushBack buf
        ACIB.unsafeFreeze buf
   in ys == zs

prop_pushPop :: [Int] -> Bool
prop_pushPop xs =
  let ys = runST $ do
        buf <- ACIB.new $ length xs
        for_ xs $ ACIB.pushBack buf
        replicateM (length xs) $ fromJust <$> ACIB.popBack buf
   in xs == reverse ys

tests :: [TestTree]
tests =
  [ QC.testProperty "build" prop_build,
    QC.testProperty "push" prop_push,
    QC.testProperty "pushPop" prop_pushPop
  ]
