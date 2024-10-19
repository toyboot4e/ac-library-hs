module Tests.Internal.GrowVec (tests) where

import AtCoder.Internal.GrowVec qualified as ACGV
import Control.Monad
import Control.Monad.ST (runST)
import Data.Foldable
import Data.Maybe
import Data.Vector.Unboxed qualified as VU
import Test.Tasty
import Test.Tasty.QuickCheck as QC

zeroNew :: [Int] -> Bool
zeroNew xs =
  let ys = VU.fromList xs
      zs = runST $ do
        buf <- ACGV.new 0
        for_ xs $ ACGV.pushBack buf
        ACGV.unsafeFreeze buf
   in ys == zs

zeroBuild :: [Int] -> Bool
zeroBuild xs =
  let ys = VU.fromList xs
      zs = runST $ do
        buf <- ACGV.build VU.empty
        for_ xs $ ACGV.pushBack buf
        ACGV.unsafeFreeze buf
   in ys == zs

pushPop :: [Int] -> Bool
pushPop xs =
  let ys = runST $ do
        buf <- ACGV.new $ length xs
        for_ xs $ ACGV.pushBack buf
        replicateM (length xs) $ fromJust <$> ACGV.popBack buf
   in xs == reverse ys

tests :: [TestTree]
tests =
  [ testGroup
      "AtCoder.Internal.GrowVec"
      [ QC.testProperty "zeroNew" zeroNew,
        QC.testProperty "zeroBuild" zeroBuild,
        QC.testProperty "pushPop" pushPop
      ]
  ]
