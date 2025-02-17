{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Tests.Extra.DynSparseSegTree where

import AtCoder.Extra.DynSparseSegTree qualified as Seg
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
    segM :: !(IO (Seg.DynSparseSegTree RealWorld (Sum Mint)))
  }

instance Show Init where
  show Init {..} = show n

instance QC.Arbitrary Init where
  arbitrary = do
    n <- QC.chooseInt (1, 256)
    q <- QC.chooseInt (1, 5 * n)
    l0 <- QC.chooseInt (-256, 256)
    let cap = Seg.recommendedCapacity n q
    pure $ Init n q (VUM.replicate n mempty) (Seg.new cap l0 (l0 + n))

data Query
  = Write !Int !(Sum Mint)
  | Modify !Int !(Sum Mint)
  | ModifyM !Int !(Sum Mint)
  | Prod !(Int, Int)
  | AllProd
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
      pure AllProd
    ]
  where
    k = QC.chooseInt (0, n - 1)
    v = Sum . modInt <$> QC.arbitrary
    i = intervalGen n

-- | containers. (referencial implementation)
handleRef :: VUM.MVector RealWorld (Sum Mint) -> Query -> IO Result
handleRef vec q = case q of
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

-- | ac-library-hs.
handleAcl :: (HasCallStack) => Seg.DynSparseSegTree RealWorld (Sum Mint) -> Seg.Handle RealWorld -> Query -> IO Result
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
  where
    d = (+ Seg.l0Dsst seg)

prop_randomTest :: Init -> QC.Property
prop_randomTest Init {..} = QCM.monadicIO $ do
  qs <- QCM.pick $ QC.vectorOf q (queryGen n)
  ref <- QCM.run refM
  seg <- QCM.run segM
  let bounds = (Seg.l0Dsst seg, Seg.r0Dsst seg)
  -- FIXME: the root is null and not updated
  root <- QCM.run $ Seg.newRoot seg
  for_ qs $ \query -> do
    expected <- QCM.run $ handleRef ref query
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
          root <- Seg.newRoot seg
          VU.iforM_ xs $ \i x -> do
            Seg.write seg root i $ Sum x
          Seg.maxRight seg root (<= Sum xRef)
     in expected QC.=== res

tests :: [TestTree]
tests =
  [ QC.testProperty "randomTest" prop_randomTest,
    QC.testProperty "maxRight" prop_maxRight
  ]
