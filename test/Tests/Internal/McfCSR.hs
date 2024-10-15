{-# LANGUAGE RecordWildCards #-}

module Tests.Internal.McfCSR (tests) where

import AtCoder.Internal.McfCSR qualified as McfCSR
import Data.Foldable
import Data.Vector.Unboxed qualified as VU
import Test.QuickCheck.Monadic as QC
import Test.Tasty
import Test.Tasty.QuickCheck as QC

-- TODO: test reverse edge direction

edgeGen :: Int -> Int -> Gen [(Int, Int, Int, Int, Int)]
edgeGen n m = QC.vectorOf m $ do
  from <- QC.chooseInt (0, n - 1)
  to <- QC.chooseInt (0, n - 1)
  cap <- QC.chooseInt (0, 16)
  flow <- QC.chooseInt (0, cap)
  cost <- QC.chooseInt (0, 16)
  return (from, to, cap, flow, cost)

-- TODO: monadic gen?
-- TODO: Deduplicate graph generation code. Implement Arbitrary?

-- genGraph :: (PrimMonad m) => Int -> Int -> m (Gen (McfCSR.CSR (PrimState m) Int Int))
-- genGraph n m = monadicIO $ do

props :: TestTree
props =
  testGroup
    "McfCS props"
    [ QC.testProperty "reverse edge direction" $ QC.monadicIO $ do
        n <- QC.pick $ QC.chooseInt (1, 16)
        m <- QC.pick $ QC.chooseInt (0, 128)
        edges <- QC.pick $ edgeGen n m
        (!_, csr@McfCSR.CSR {..}) <- QC.run $ McfCSR.build n (VU.fromList edges)
        for_ [0 .. n - 1] $ \from -> do
          VU.forM_ (McfCSR.adj csr from) $ \(!_to, !rev, !cost) -> do
            -- test reverse edge direction
            assert $ toCSR VU.! rev == from
            -- test reverse edge cost
            assert $ -costCSR VU.! rev == cost
    ]

tests :: [TestTree]
tests = [props]
