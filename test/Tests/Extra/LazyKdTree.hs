{-# LANGUAGE RecordWildCards #-}

module Tests.Extra.LazyKdTree where

import AtCoder.Extra.LazyKdTree qualified as Lkt
import AtCoder.Extra.Monoid.Affine1 (Affine1 (..))
import AtCoder.Extra.Monoid.Affine1 qualified as Affine1
import AtCoder.LazySegTree (segAct)
import AtCoder.ModInt qualified as M
import Control.Monad (when)
import Control.Monad.ST (RealWorld)
import Data.Foldable (for_)
import Data.Semigroup (Sum (..))
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import Test.QuickCheck.Monadic as QCM
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Tests.Util

type Mint = M.ModInt998244353

modInt :: Int -> Mint
modInt = M.new

data Init = Init
  { n :: {-# UNPACK #-} !Int,
    q :: {-# UNPACK #-} !Int,
    refM :: !(IO (VUM.MVector RealWorld (Int, Int, Sum Mint))),
    ktM :: !(IO (Lkt.LazyKdTree RealWorld (Affine1 Mint) (Sum Mint)))
  }

rng :: (Int, Int)
rng = (-rngI, rngI)

rngI :: Int
rngI = 10

instance Show Init where
  show Init {..} = show n

instance QC.Arbitrary Init where
  arbitrary = do
    n <- QC.chooseInt (1, 256)
    q <- QC.chooseInt (1, 1024)
    xyws <- (VU.fromList <$>) $ QC.vectorOf n $ do
      x <- QC.chooseInt rng
      y <- QC.chooseInt rng
      w <- Sum . M.new <$> QC.arbitrary @Int
      pure (x, y, w)
    let refM = VU.thaw xyws
    let ktM = Lkt.build3 xyws
    pure Init {..}

data Query
  = Write !Int !(Sum Mint)
  | Modify !Int !(Sum Mint)
  | ModifyM !Int !(Sum Mint)
  | Affine !((Int, Int), (Int, Int)) !(Affine1 Mint)
  | Prod !((Int, Int), (Int, Int))
  | AllProd
  deriving (Show)

-- | Arbitrary return type for the `Query` result.
data Result
  = None
  | S !(Sum Mint)
  deriving (Show, Eq)

queryGen :: Int -> QC.Gen Query
queryGen n = do
  QC.oneof
    [ Write <$> i <*> v,
      Modify <$> i <*> v,
      ModifyM <$> i <*> v,
      Affine <$> r <*> f,
      Prod <$> r,
      pure AllProd
    ]
  where
    i = QC.chooseInt (0, n - 1)
    r = (,) <$> intervalGen' (-rngI) rngI <*> intervalGen' (-rngI) rngI
    v = Sum . modInt <$> QC.arbitrary
    f = Affine1.new <$> (modInt <$> QC.arbitrary) <*> (modInt <$> QC.arbitrary)

-- | containers. (referencial implementation)
handleRef :: VUM.MVector RealWorld (Int, Int, Sum Mint) -> Query -> IO Result
handleRef vec q = case q of
  Write i v -> do
    VGM.write ws i v
    pure None
  Modify k v -> do
    VGM.modify ws (+ v) k
    pure None
  ModifyM k v -> do
    VGM.modify ws (+ v) k
    pure None
  Affine ((!xl, !xr), (!yl, !yr)) f -> do
    xys <- VU.unsafeFreeze $ VUM.zip xs ys
    VU.iforM_ xys $ \i (!x, !y) -> do
      when (xl <= x && x < xr && yl <= y && y < yr) $ do
        VGM.modify ws (segAct f) i
    pure None
  Prod ((!xl, !xr), (!yl, !yr)) -> do
    S
      . VU.foldl'
        ( \acc (!x, !y, !v) ->
            if xl <= x && x < xr && yl <= y && y < yr then acc <> v else acc
        )
        mempty
      <$> VU.unsafeFreeze vec
  AllProd -> do
    S <$> VGM.foldl' (<>) mempty ws
  where
    (!xs, !ys, !ws) = VUM.unzip3 vec

-- | ac-library-hs.
handleAcl :: (HasCallStack) => Lkt.LazyKdTree RealWorld (Affine1 Mint) (Sum Mint) -> Query -> IO Result
handleAcl kt q = case q of
  Write i v -> do
    Lkt.write kt i v
    pure None
  Modify i v -> do
    Lkt.modify kt (+ v) i
    pure None
  ModifyM i v -> do
    Lkt.modifyM kt (pure . (+ v)) i
    pure None
  Affine ((!xl, !xr), (!yl, !yr)) f -> do
    Lkt.applyIn kt xl xr yl yr f
    pure None
  Prod ((!xl, !xr), (!yl, !yr)) -> do
    S <$> Lkt.prod kt xl xr yl yr
  AllProd -> do
    S <$> Lkt.allProd kt

prop_randomTest :: Init -> QC.Property
prop_randomTest Init {..} = QCM.monadicIO $ do
  qs <- QCM.pick $ QC.vectorOf q (queryGen n)
  vec <- QCM.run refM
  kt <- QCM.run ktM
  for_ qs $ \query -> do
    expected <- QCM.run $ handleRef vec query
    res <- QCM.run $ handleAcl kt query
    QCM.assertWith (expected == res) $ show (query, expected, res)

unit_zero :: TestTree
unit_zero = testCase "zero" $ do
  kt <- Lkt.build @_ @(Affine1 Mint) @(Sum Mint) VU.empty VU.empty VU.empty
  Lkt.applyIn kt 0 10 0 10 $ Affine1.new 1 1
  (@?= mempty) =<< Lkt.prod kt 0 10 0 10
  (@?= mempty) =<< Lkt.allProd kt
  pure ()

tests :: [TestTree]
tests =
  [ QC.testProperty "randomTest" prop_randomTest,
    unit_zero
  ]
