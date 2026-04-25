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

-- | Tests swap on a queue after popFront (so l > 0), verifying the bounds
-- check uses logical 0-based indices.
prop_swap :: QC.Gen QC.Property
prop_swap = do
  (!n, !xs, !nPop, !i, !j) <- gen
  let result = runST $ do
        buf <- ACIQ.new n
        for_ xs $ ACIQ.pushBack buf
        for_ [1 .. nPop] $ \_ -> ACIQ.popFront_ buf
        ACIQ.swap buf i j
        ACIQ.freeze buf
      expected =
        let v = VU.fromList (drop nPop xs)
         in v VU.// [(i, v VU.! j), (j, v VU.! i)]
  pure $ result QC.=== expected
  where
    gen = do
      n <- QC.chooseInt (2, 20)
      xs <- QC.vectorOf n (QC.arbitrary @Int)
      nPop <- QC.chooseInt (0, n - 2)
      let remaining = n - nPop
      i <- QC.chooseInt (0, remaining - 1)
      j <- QC.chooseInt (0, remaining - 1)
      pure (n, xs, nPop, i, j)

tests :: [TestTree]
tests =
  [ QC.testProperty "push" prop_push,
    QC.testProperty "swap" prop_swap
  ]
