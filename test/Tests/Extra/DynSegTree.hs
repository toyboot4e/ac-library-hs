{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Tests.Extra.DynSegTree where

import AtCoder.Extra.DynSegTree qualified as Seg
import AtCoder.ModInt qualified as M
import Control.Monad.ST (RealWorld, runST)
import Data.Foldable (for_)
import Data.Semigroup (Sum (..))
import Data.Vector.Algorithms.Intro qualified as VAI
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Stack (HasCallStack)
import Test.QuickCheck.Monadic as QCM
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Tests.Util

type Mint = M.ModInt998244353

modInt :: Int -> Mint
modInt = M.new

data Init = Init
  { n :: {-# UNPACK #-} !Int,
    q :: {-# UNPACK #-} !Int,
    refM :: !(IO (VUM.MVector RealWorld (Sum Mint))),
    segM :: !(IO (Seg.DynSegTree RealWorld (Sum Mint)))
  }

instance Show Init where
  show Init {..} = show n

initialProd :: Int -> Int -> Sum Mint
initialProd l r = Sum . modInt $ sum [l .. r - 1]

instance QC.Arbitrary Init where
  arbitrary = do
    n <- QC.chooseInt (1, 256)
    q <- QC.chooseInt (1, 5 * n)
    l0 <- QC.chooseInt (-256, 256)
    -- initial node values: l0, l0 + 1, l0 + 2, ..
    let cap = Seg.recommendedCapacity n q
    pure
      . Init n q (VUM.generate n (\i -> initialProd (i + l0) (i + l0 + 1)))
      $ Seg.buildWith cap l0 (l0 + n) initialProd

data Query
  = Write !Int !(Sum Mint)
  | Modify !Int !(Sum Mint)
  | ModifyM !Int !(Sum Mint)
  | Prod !(Int, Int)
  | AllProd
  | ResetInterval !(Int, Int)
  deriving (Show)

-- | Arbitrary return type for the `Query` result.
data Result
  = None
  | S !(Sum Mint)
  | F !(VU.Vector (Sum Mint))
  deriving (Show, Eq)

queryGen :: Int -> QC.Gen Query
queryGen n = do
  QC.oneof
    [ Write <$> k <*> v,
      Modify <$> k <*> v,
      ModifyM <$> k <*> v,
      Prod <$> i,
      pure AllProd,
      ResetInterval <$> i
    ]
  where
    k = QC.chooseInt (0, n - 1)
    v = Sum . modInt <$> QC.arbitrary
    i = intervalGen n

-- | containers. (referencial implementation)
handleRef :: Int -> VUM.MVector RealWorld (Sum Mint) -> Query -> IO Result
handleRef l0 vec q = case q of
  Write k v -> do
    VGM.write vec k v
    pure None
  Modify k v -> do
    VGM.modify vec (+ v) k
    pure None
  ModifyM k v -> do
    VGM.modify vec (+ v) k
    pure None
  Prod (!l, !r) -> do
    (S <$>) . VGM.foldl' (<>) mempty $ VGM.slice l (r - l) vec
  AllProd -> do
    S <$> VGM.foldl' (<>) mempty vec
  ResetInterval (!l, !r) -> do
    for_ [l .. r - 1] $ \i -> do
      VGM.write vec i $! initialProd (l0 + i) (l0 + i + 1)
    pure None

-- | ac-library-hs.
handleAcl :: (HasCallStack) => Seg.DynSegTree RealWorld (Sum Mint) -> Seg.Index -> Query -> IO Result
handleAcl seg root q = case q of
  Write (d -> k) v -> do
    Seg.write seg root k v
    pure None
  Modify (d -> k) v -> do
    Seg.modify seg root (+ v) k
    pure None
  ModifyM (d -> k) v -> do
    Seg.modifyM seg root (pure . (+ v)) k
    pure None
  Prod (d -> l, d -> r) -> do
    S <$> Seg.prod seg root l r
  AllProd -> do
    S <$> Seg.allProd seg root
  ResetInterval (d -> l, d -> r) -> do
    Seg.resetInterval seg root l r
    pure None
  where
    d = (+ Seg.l0Dst seg)

prop_randomTest :: Init -> QC.Property
prop_randomTest Init {..} = QCM.monadicIO $ do
  qs <- QCM.pick $ QC.vectorOf q (queryGen n)
  ref <- QCM.run refM
  seg <- QCM.run segM
  let l0 = Seg.l0Dst seg
  let bounds = (Seg.l0Dst seg, Seg.r0Dst seg)
  root <- QCM.run $ Seg.newRoot seg
  for_ qs $ \query -> do
    expected <- QCM.run $ handleRef l0 ref query
    res <- QCM.run $ handleAcl seg root query
    QCM.assertWith (expected == res) $ show (bounds, query, expected, res)

-- prop_foldl is tested with large array verification problem

prop_maxRight :: Int -> [QC.NonNegative Int] -> QC.Property
prop_maxRight xRef xs_ =
  not (null xs_) QC.==> do
    let xs = VU.modify VAI.sort $ VU.fromList $ map (\(QC.NonNegative x) -> x) xs_
        expected = VU.length . VU.takeWhile (<= xRef) $ VU.scanl1' (+) xs
        res = runST $ do
          seg <- Seg.new @_ @(Sum Int) (2 * VU.length xs) 0 (VU.length xs)
          root <- Seg.newSeq seg $ VU.map Sum xs
          Seg.maxRight seg root (<= Sum xRef)
     in expected QC.=== res

tests :: [TestTree]
tests =
  [ QC.testProperty "randomTest" prop_randomTest,
    QC.testProperty "maxRight" prop_maxRight
  ]
