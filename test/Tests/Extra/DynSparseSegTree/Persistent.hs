{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Tests.Extra.DynSparseSegTree.Persistent where

import AtCoder.Extra.DynSparseSegTree.Persistent qualified as Seg
import AtCoder.ModInt qualified as M
import Control.Monad.ST (RealWorld, runST)
import Data.Foldable (for_)
import Data.Semigroup (Sum (..))
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
    -- initial node values: l0, l0 + 1, l0 + 2, ..
    let cap = Seg.recommendedCapacity n q
    pure $ Init n q (VUM.replicate n 0) (Seg.new cap l0 (l0 + n))

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
handleAcl :: (HasCallStack) => Seg.DynSparseSegTree RealWorld (Sum Mint) -> VUM.MVector RealWorld Seg.Index -> Query -> IO Result
handleAcl seg rootVec q = do
  root <- VGM.read rootVec 0
  case q of
    Write (d -> k) v -> do
      VGM.write rootVec 0 =<< Seg.write seg root k v
      VGM.write rootVec 0 =<< Seg.write seg root k v
      pure None
    Modify (d -> k) v -> do
      VGM.write rootVec 0 =<< Seg.modify seg root (+ v) k
      pure None
    ModifyM (d -> k) v -> do
      VGM.write rootVec 0 =<< Seg.modifyM seg root (pure . (+ v)) k
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
  root <- QCM.run $ Seg.newRoot seg
  rootVec <- QCM.run $ VUM.replicate 1 root
  for_ qs $ \query -> do
    expected <- QCM.run $ handleRef ref query
    res <- QCM.run $ handleAcl seg rootVec query
    QCM.assertWith (expected == res) $ show (bounds, query, expected, res)

-- prop_foldl is tested with large array verification problem

prop_maxRight :: Int -> QC.NonEmptyList (QC.NonNegative Int) -> QC.Property
prop_maxRight xRef (QC.NonEmpty xs_) =
  let xs = VU.fromList $ map (\(QC.NonNegative x) -> x) xs_
      expected = VU.length . VU.takeWhile (<= xRef) $ VU.scanl1' (+) xs
      res = runST $ do
        seg <- Seg.new @_ @(Sum Int) (Seg.recommendedCapacity (VU.length xs) (VU.length xs)) 0 (VU.length xs)
        root0 <- Seg.newRoot seg
        root <- VU.ifoldM' (Seg.write seg) root0 $ VU.map Sum xs
        Seg.maxRight seg root (<= Sum xRef)
   in expected QC.=== res

tests :: [TestTree]
tests =
  [ QC.testProperty "randomTest" prop_randomTest,
    QC.testProperty "maxRight" prop_maxRight
  ]
