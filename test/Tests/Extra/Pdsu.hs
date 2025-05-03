{-# LANGUAGE DerivingStrategies #-}

module Tests.Extra.Pdsu (tests) where

import AtCoder.Extra.Pdsu qualified as Pdsu
import Control.Monad (forM)
import Control.Monad.ST (runST)
import Data.Maybe (fromJust)
import Test.Tasty
import Test.Tasty.QuickCheck as QC

prop_merge :: QC.Positive Int -> QC.Gen QC.Property
prop_merge (QC.Positive n) = do
  m <- QC.chooseInt (1, 2 * n)
  es <- QC.vectorOf m $ do
    u <- QC.chooseInt (0, n - 1)
    v <- QC.chooseInt (0, n - 1)
    pure (u, v)
  pure . QC.conjoin $ runST $ do
    dsu <- Pdsu.new @_ @() n id
    forM es $ \(!u, !v) -> do
      r' <- Pdsu.merge dsu u v ()
      r1 <- Pdsu.leader dsu u
      r2 <- Pdsu.leader dsu v
      pure (r1 == r' && r2 == r')

prop_mergeMaybe :: QC.Positive Int -> QC.Gen QC.Property
prop_mergeMaybe (QC.Positive n) = do
  m <- QC.chooseInt (1, 2 * n)
  es <- QC.vectorOf m $ do
    u <- QC.chooseInt (0, n - 1)
    v <- QC.chooseInt (0, n - 1)
    pure (u, v)
  pure . QC.conjoin $ runST $ do
    dsu <- Pdsu.new @_ @() n id
    forM es $ \(!u, !v) -> do
      r1 <- Pdsu.leader dsu u
      r2 <- Pdsu.leader dsu v
      r' <- Pdsu.mergeMaybe dsu u v ()
      if r1 == r2
        then pure $ r' == Nothing
        else do
          r1' <- Pdsu.leader dsu u
          r2' <- Pdsu.leader dsu v
          pure (r1' == fromJust r' && r2' == fromJust r')

tests :: [TestTree]
tests =
  [ QC.testProperty "merge" prop_merge,
    QC.testProperty "mergeMaybe" prop_mergeMaybe
  ]
