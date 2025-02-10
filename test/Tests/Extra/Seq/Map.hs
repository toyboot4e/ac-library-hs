{-# LANGUAGE RecordWildCards #-}

module Tests.Extra.Seq.Map (tests) where

import AtCoder.Extra.Monoid.Affine1 (Affine1 (..))
import AtCoder.Extra.Monoid.Affine1 qualified as Affine1
import AtCoder.Extra.Pool qualified as P
import AtCoder.Extra.Seq.Map qualified as Map
import AtCoder.Internal.Assert qualified as ACIA
import Control.Monad (foldM_, when)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST (RealWorld, runST)
import Data.Foldable (toList)
import Data.List qualified as L
import Data.Map qualified as M
import Data.Semigroup (Sum (..))
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec
import Test.QuickCheck.Monadic as QCM
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck as QC
import Tests.Util
import Prelude hiding (seq)

data Init = Init
  { n :: {-# UNPACK #-} !Int,
    q :: {-# UNPACK #-} !Int,
    ref0 :: !(M.Map Int (Sum Int)),
    mapM :: !(IO (Map.Map RealWorld (Affine1 Int) Int (Sum Int)))
  }

instance Show Init where
  show Init {..} = show (n, ref0)

instance QC.Arbitrary Init where
  arbitrary = do
    n <- QC.chooseInt (1, 64)
    q <- QC.chooseInt (1, 5 * n)
    pure $ Init n q M.empty $ Map.new (n + q)

data Query
  = Reset
  | Insert !Int !(Sum Int)
  | Delete !Int
  | Delete_ !Int
  | Member !Int
  | LookupLE !Int
  | LookupGE !Int
  | Freeze
  deriving (Show)

-- | Arbitrary return type for the `Query` result.
data Result
  = None
  | B !Bool
  | I !Int
  | S !(Sum Int)
  | MKV !(Maybe (Int, Sum Int))
  | MS !(Maybe (Sum Int))
  | F !(VU.Vector (Int, Sum Int))
  deriving (Show, Eq)

queryGen :: QC.Gen Query
queryGen = do
  QC.frequency
    [ (rare, pure Reset),
      (often, Member <$> keyGen),
      (often, Insert <$> keyGen <*> valGen),
      (half, Delete <$> keyGen),
      (half, Delete_ <$> keyGen),
      (often, LookupLE <$> keyGen),
      (often, LookupGE <$> keyGen),
      (rare, pure Freeze)
    ]
  where
    rare = 1
    often = 10
    half = 5
    keyGen = QC.chooseInt (-30, 30)
    -- use non-negative values for monotoniously increasing sum
    valGen = Sum <$> QC.chooseInt (0, 10)
    -- NOTE: it might throw an error on overflow:
    fGen = Affine1.new <$> QC.chooseInt (0, 4) <*> QC.chooseInt (0, 4)

-- | containers. (referencial implementation)
handleRef :: M.Map Int (Sum Int) -> Query -> (M.Map Int (Sum Int), Result)
handleRef m q = case q of
  Reset -> (M.empty, None)
  Insert k v -> (M.insert k v m, None)
  Delete k -> (M.delete k m, MS $ M.lookup k m)
  Delete_ k -> (M.delete k m, None)
  Member k -> (m, B (M.member k m))
  LookupLE k -> (m, MKV $ M.lookupLE k m)
  LookupGE k -> (m, MKV $ M.lookupGE k m)
  Freeze -> (m, F (VU.fromList (M.assocs m)))

-- | ac-library-hs.
handleAcl :: (HasCallStack, PrimMonad m) => Map.Map (PrimState m) (Affine1 Int) Int (Sum Int) -> Query -> m Result
handleAcl m q = case q of
  Reset -> do
    Map.reset m
    pure None
  Insert k v -> do
    Map.insert m k v
    pure None
  Delete k -> do
    MS <$> Map.delete m k
  Delete_ k -> do
    Map.delete_ m k
    pure None
  Member k -> do
    B <$> Map.member m k
  LookupLE v -> do
    MKV <$> Map.lookupLE m v
  LookupGE v -> do
    MKV <$> Map.lookupGE m v
  Freeze -> do
    F <$> Map.freeze m

prop_randomTest :: Init -> QC.Property
prop_randomTest Init {..} = QCM.monadicIO $ do
  m <- QCM.run mapM
  foldM_
    ( \ref _ -> do
        query <- QCM.pick queryGen
        -- run the query
        let (!ref', !expected) = handleRef ref query
        res <- QCM.run $ handleAcl m query
        QCM.assertWith (expected == res) $ show (query, expected, res)
        pure ref'
    )
    ref0
    [0 .. q - 1]

tests :: [TestTree]
tests =
  [ QC.testProperty "random test" prop_randomTest
  ]
