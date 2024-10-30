module Tests.Internal.GrowVec (tests) where

import AtCoder.Internal.GrowVec qualified as ACGV
import Control.Monad
import Control.Monad.ST (runST)
import Data.Foldable
import Data.Maybe
import Data.Vector.Unboxed qualified as VU
import Test.Tasty
import Test.Tasty.QuickCheck as QC

prop_zeroNew :: [Int] -> Bool
prop_zeroNew xs =
  let ys = VU.fromList xs
      zs = runST $ do
        buf <- ACGV.new 0
        for_ xs $ ACGV.pushBack buf
        ACGV.unsafeFreeze buf
   in ys == zs

prop_zeroBuild :: [Int] -> Bool
prop_zeroBuild xs =
  let ys = VU.fromList xs
      zs = runST $ do
        buf <- ACGV.build VU.empty
        for_ xs $ ACGV.pushBack buf
        ACGV.unsafeFreeze buf
   in ys == zs

prop_pushPop :: [Int] -> Bool
prop_pushPop xs =
  let ys = runST $ do
        buf <- ACGV.new $ length xs
        for_ xs $ ACGV.pushBack buf
        replicateM (length xs) $ fromJust <$> ACGV.popBack buf
   in xs == reverse ys

tests :: [TestTree]
tests =
  [ testGroup
      "AtCoder.Internal.GrowVec"
      [ QC.testProperty "zeroNew" prop_zeroNew,
        QC.testProperty "zeroBuild" prop_zeroBuild,
        QC.testProperty "pushPop" prop_pushPop
      ]
  ]
