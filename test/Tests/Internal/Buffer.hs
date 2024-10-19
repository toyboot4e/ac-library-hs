module Tests.Internal.Buffer (tests) where

import AtCoder.Internal.Buffer qualified as ACB
import Control.Monad
import Control.Monad.ST (runST)
import Data.Foldable
import Data.Maybe
import Data.Vector.Unboxed qualified as VU
import Test.Tasty
import Test.Tasty.QuickCheck as QC

build :: [Int] -> Bool
build xs =
  let ys = VU.fromList xs
      zs = runST $ do
        buf <- ACB.build $ VU.fromList xs
        ACB.unsafeFreeze buf
   in ys == zs

push :: [Int] -> Bool
push xs =
  let ys = VU.fromList xs
      zs = runST $ do
        buf <- ACB.new $ length xs
        for_ xs $ ACB.pushBack buf
        ACB.unsafeFreeze buf
   in ys == zs

pushPop :: [Int] -> Bool
pushPop xs =
  let ys = runST $ do
        buf <- ACB.new $ length xs
        for_ xs $ ACB.pushBack buf
        replicateM (length xs) $ fromJust <$> ACB.popBack buf
   in xs == reverse ys

tests :: [TestTree]
tests =
  [ testGroup
      "AtCoder.Internal.Buffer"
      [ QC.testProperty "build" build,
        QC.testProperty "push" push,
        QC.testProperty "pushPop" pushPop
      ]
  ]
