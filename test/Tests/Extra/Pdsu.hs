{-# LANGUAGE DerivingStrategies #-}

module Tests.Extra.Pdsu (tests) where

import AtCoder.Extra.Pdsu qualified as Pdsu
import Control.Monad (forM)
import Control.Monad.ST (runST)
import Data.Foldable (for_)
import Data.Maybe (fromJust)
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
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
      su <- Pdsu.size dsu u
      sv <- Pdsu.size dsu v
      pure $
        QC.conjoin
          [ r1 == r' QC..&&. r2 == r',
            QC.counterexample ("size u /= size v after merge: " ++ show (su, sv)) $ su == sv
          ]

prop_size :: QC.Positive Int -> QC.Gen QC.Property
prop_size (QC.Positive n) = do
  m <- QC.chooseInt (1, 2 * n)
  es <- QC.vectorOf m $ do
    u <- QC.chooseInt (0, n - 1)
    v <- QC.chooseInt (0, n - 1)
    pure (u, v)
  pure . QC.conjoin $ runST $ do
    dsu <- Pdsu.new @_ @() n id
    for_ es $ \(!u, !v) -> Pdsu.merge dsu u v ()
    refSizes <- VU.accumulate (+) (VU.replicate n 0) <$> VU.generateM n (fmap (,1) . Pdsu.leader dsu)
    forM [0 .. n - 1] $ \i -> do
      l <- Pdsu.leader dsu i
      s <- Pdsu.size dsu i
      let expected = refSizes VU.! l
      pure $ QC.counterexample ("vertex " ++ show i ++ ": size=" ++ show s ++ " expected=" ++ show expected) $ s == expected

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
    QC.testProperty "mergeMaybe" prop_mergeMaybe,
    QC.testProperty "size" prop_size
  ]
