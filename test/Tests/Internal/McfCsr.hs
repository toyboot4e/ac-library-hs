{-# LANGUAGE RecordWildCards #-}

module Tests.Internal.McfCsr (tests) where

import AtCoder.Internal.McfCsr qualified as ACIMCSR
import Data.Foldable
import Data.Vector.Unboxed qualified as VU
import Test.QuickCheck.Monadic qualified as QC
import Test.Tasty
import Test.Tasty.QuickCheck qualified as QC

-- TODO: test reverse edge direction

edgeGen :: Int -> Int -> QC.Gen [(Int, Int, Int, Int, Int)]
edgeGen n m = QC.vectorOf m $ do
  from <- QC.chooseInt (0, n - 1)
  to <- QC.chooseInt (0, n - 1)
  cap <- QC.chooseInt (0, 16)
  flow <- QC.chooseInt (0, cap)
  cost <- QC.chooseInt (0, 16)
  pure (from, to, cap, flow, cost)

-- TODO: monadic gen?
-- TODO: Deduplicate graph generation code. Implement Arbitrary?

-- genGraph :: (PrimMonad m) => Int -> Int -> m (Gen (ACIMCSR.Csr (PrimState m) Int Int))
-- genGraph n m = monadicIO $ do

props :: TestTree
props =
  testGroup
    "McfCS props"
    [ QC.testProperty "reverse edge direction" $ QC.monadicIO $ do
        n <- QC.pick $ QC.chooseInt (1, 16)
        m <- QC.pick $ QC.chooseInt (0, 128)
        edges <- QC.pick $ edgeGen n m
        (!_, csr@ACIMCSR.Csr {..}) <- QC.run $ ACIMCSR.build n (VU.fromList edges)
        for_ [0 .. n - 1] $ \from -> do
          VU.forM_ (ACIMCSR.adj csr from) $ \(!_to, !rev, !cost) -> do
            -- test reverse edge direction
            QC.assert $ toCsr VU.! rev == from
            -- test reverse edge cost
            QC.assert $ -costCsr VU.! rev == cost
    ]

tests :: [TestTree]
tests = [props]
