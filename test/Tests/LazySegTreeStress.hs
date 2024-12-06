{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Tests.LazySegTreeStress (tests) where

import AtCoder.LazySegTree qualified as LST
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.State (liftIO)
import Control.Monad.State.Strict (StateT (..))
import Data.Foldable
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import System.Random.Stateful
import Test.Tasty
import Test.Tasty.HUnit

-- | Naive implementation of lazy segment tree.
newtype TimeManager s = TimeManager (VUM.MVector s Int)

action :: (PrimMonad m) => TimeManager (PrimState m) -> Int -> Int -> Int -> m ()
action (TimeManager vec) l r newTime = do
  for_ [l .. r - 1] $ \i -> do
    VUM.write vec i newTime

prod :: (PrimMonad m) => TimeManager (PrimState m) -> Int -> Int -> m Int
prod (TimeManager vec) l r =
  VU.foldM'
    ( \ !acc i -> do
        x <- VUM.read vec i
        pure $! max acc x
    )
    (-1 :: Int)
    (VU.generate (r - l) (+ l))

-- | S (l, r, time): half-open interval with generation.
newtype S = S SRepr
  deriving (Eq, Ord, Show)

type SRepr = (Int, Int, Int)

instance Semigroup S where
  l@(S (!l1, !_, !time1)) <> r@(S (!l2, !r2, !time2))
    | l1 == -1 = r
    | l2 == -1 = l
    | otherwise = S (l1, r2, max time1 time2)

instance Monoid S where
  mempty = S (-1, -1, -1)

newtype instance VU.MVector s S = MV_S (VU.MVector s SRepr)

newtype instance VU.Vector S = V_S (VU.Vector SRepr)

deriving instance VGM.MVector VUM.MVector S

deriving instance VG.Vector VU.Vector S

instance VU.Unbox S

-- | T newTime: generation.
newtype T = T TRepr
  deriving (Eq, Ord, Show)

type TRepr = Int

instance Semigroup T where
  -- new <> old??
  (T t1) <> r@(T t2)
    | t1 == -1 = r
    | t1 <= t2 = error "unreachable"
    | otherwise = T t1

instance Monoid T where
  mempty = T (-1)

instance LST.SegAct T S where
  segAct t@(T newTime) s@(S (!l, !r, !time))
    | t == mempty = s
    | time >= newTime = error "unreachable"
    | otherwise = S (l, r, newTime)

newtype instance VU.MVector s T = MV_T (VU.MVector s TRepr)

newtype instance VU.Vector T = V_T (VU.Vector TRepr)

deriving instance VGM.MVector VUM.MVector T

deriving instance VG.Vector VU.Vector T

instance VU.Unbox T

-- | Random choice of 2 disjoint elements from [lower, upper] (l < r)
uniformPairM :: (StatefulGen g m) => (Int, Int) -> g -> m (Int, Int)
uniformPairM rng@(!lower, !upper) g
  | upper - lower < 1 = error "invalid argument"
  | otherwise = do
      a <- uniformRM rng g
      b <- uniformRM rng g
      if a == b
        then uniformPairM rng g -- retry
        else pure (min a b, max a b)

-- TODO: write type?
-- testDriver :: Int ->
--    (Int -> LST.LazySegTree (PrimState (StateT UniformGen IO )
testDriver ::
  (PrimMonad m) =>
  (Int, Int) ->
  Int ->
  (Int -> LST.LazySegTree (PrimState m) T S -> TimeManager (PrimState m) -> Int -> Int -> Int -> StateT StdGen m Int) ->
  m ()
testDriver tyRange@(!_, !_) nRepeat f = do
  for_ [1 .. 30] $ \n -> do
    for_ [1 .. 10 - 1] $ \_ -> do
      seg0 <- LST.build $ VU.generate n $ \i -> S (i, i + 1, -1)
      tm <- TimeManager <$> VUM.replicate n (-1)
      runStateGenT_ (mkStdGen 42) $ \g -> do
        VU.foldM'_
          ( \now _ -> do
              ty :: Int <- uniformRM tyRange g
              (!l, !r) <- uniformPairM (0, n) g
              f now seg0 tm ty l r
          )
          (0 :: Int)
          (VU.generate nRepeat id)

-- | prod, read, applyIn, applyAt
unit_naiveTest :: TestTree
unit_naiveTest = testCase "naiveTest" $
  testDriver (0, 3) 3000 $ \now seg0 tm ty l r -> case ty of
    0 -> do
      -- prod
      S (!resL, !resR, !resTime) <- LST.prod seg0 l r
      liftIO $ l @=? resL
      liftIO $ r @=? resR
      liftIO . (@=? resTime) =<< prod tm l r
      pure now
    1 -> do
      -- read
      S (!resL, !resR, !resTime) <- LST.read seg0 l
      liftIO $ l @=? resL
      liftIO $ l + 1 @=? resR
      liftIO . (@=? resTime) =<< prod tm l (l + 1)
      pure now
    2 -> do
      -- applyIn
      LST.applyIn seg0 l r (T (now + 1))
      action tm l r (now + 1)
      pure $ now + 1
    3 -> do
      -- applyAt
      LST.applyAt seg0 l (T (now + 1))
      action tm l (l + 1) (now + 1)
      pure $ now + 1
    _ -> error "unreachable"

unit_maxRightTest :: TestTree
unit_maxRightTest = testCase "maxRightTest" $ do
  testDriver (0, 2) 1000 $ \now seg0 tm ty l r -> case ty of
    0 -> do
      -- maxRight
      LST.maxRight seg0 l $ \(S (!lS, !rS, !_)) -> case lS of
        _
          | lS == -1 -> True
          | lS /= l -> error "unreachable"
          -- FIXME: add `maxRightM`
          -- \| sTime /= prod tm l sR
          | otherwise -> rS <= r
      pure now
    _ -> do
      LST.applyIn seg0 l r $ T (now + 1)
      action tm l r $ now + 1
      pure $ now + 1

unit_minLeftTest :: TestTree
unit_minLeftTest = testCase "minLeftTest" $ do
  testDriver (0, 2) 1000 $ \now seg0 tm ty l r -> case ty of
    0 -> do
      -- minLeft
      LST.minLeft seg0 r $ \(S (!lS, !rS, !_)) -> case lS of
        _
          | lS == -1 -> True
          | rS /= r -> error "unreachable"
          -- FIXME: add `maxRightM`
          -- \| sTime /= prod tm sL r
          | otherwise -> l <= lS
      pure now
    _ -> do
      LST.applyIn seg0 l r $ T (now + 1)
      action tm l r $ now + 1
      pure $ now + 1

tests :: [TestTree]
tests =
  [ unit_naiveTest,
    unit_maxRightTest,
    unit_minLeftTest
  ]
