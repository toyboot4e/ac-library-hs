{-# LANGUAGE RecordWildCards #-}

module Tests.Extra.Seq.Map (tests) where

import AtCoder.Extra.Monoid.Affine1 (Affine1 (..))
import AtCoder.Extra.Monoid.Affine1 qualified as Affine1
import AtCoder.Extra.Seq.Map qualified as Map
import AtCoder.LazySegTree (SegAct (..))
import Control.Monad (foldM_)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST (RealWorld, runST)
import Data.List qualified as L
import Data.Map qualified as M
import Data.Semigroup (Sum (..))
import Data.Vector.Algorithms.Intro qualified as VAI
import Data.Vector.Unboxed qualified as VU
import Test.Hspec
import Test.QuickCheck.Monadic as QCM
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Tests.Util (intervalGen)
import Prelude hiding (seq)

data Init = Init
  { q :: {-# UNPACK #-} !Int,
    ref0 :: !(M.Map Int (Sum Int)),
    mapM :: !(IO (Map.Map RealWorld (Affine1 Int) Int (Sum Int)))
  }

instance Show Init where
  show Init {..} = show ref0

instance QC.Arbitrary Init where
  arbitrary = do
    q <- QC.chooseInt (1, 640)
    pure $ Init q M.empty $ Map.new q

data Query
  = Reset
  | Capacity
  | Size
  | Member !Int
  | Adjust !Int !(Sum Int)
  | Insert !Int !(Sum Int)
  | InsertWith !Int !(Sum Int)
  | Delete !Int
  | Delete_ !Int
  | Prod !(Int, Int)
  | ProdMaybe !(Int, Int)
  | ProdAll
  | ApplyIn !(Int, Int) !(Affine1 Int)
  | ApplyAll !(Affine1 Int)
  | LookupLE !Int
  | LookupLT !Int
  | LookupGE !Int
  | LookupGT !Int
  | ReadAt !Int
  | ReadMaybeAt !Int
  | WriteAt !Int !(Sum Int)
  | ExchangeAt !Int !(Sum Int)
  | ProdInInterval !(Int, Int)
  | ApplyInInterval !(Int, Int) !(Affine1 Int)
  | -- | ILowerBound !(Sum Int)
    -- | ILowerBoundM !(Sum Int)
    ILowerBoundProd !(Sum Int)
  | ILowerBoundProdM !(Sum Int)
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

queryGen :: Int -> QC.Gen Query
queryGen n = do
  QC.frequency $
    [ (rare, pure Reset),
      (rare, pure Capacity),
      (rare, pure Size),
      (often, Member <$> keyGen),
      (often, Adjust <$> keyGen <*> valGen),
      (half, Insert <$> keyGen <*> valGen),
      (half, InsertWith <$> keyGen <*> valGen),
      (half, Delete <$> keyGen),
      (half, Delete_ <$> keyGen),
      (half, Prod <$> keyGen2),
      (half, (ProdMaybe .) . (,) <$> keyGen <*> keyGen),
      (half, pure ProdAll),
      (often, ApplyIn <$> keyGen2 <*> fGen),
      (often, ApplyAll <$> fGen),
      (half, LookupLE <$> keyGen),
      (half, LookupLT <$> keyGen),
      (half, LookupGE <$> keyGen),
      (half, LookupGT <$> keyGen),
      -- (half, ReadAt <$> iGen),
      (half, ReadMaybeAt <$> maybeIGen),
      -- (half, WriteAt <$> iGen <*> valGen),
      -- (half, ExchangeAt <$> iGen <*> valGen),
      (half, ProdInInterval <$> intervalGen n),
      (often, ApplyInInterval <$> intervalGen n <*> fGen),
      (half, ILowerBoundProd <$> valGen),
      (half, ILowerBoundProdM <$> valGen),
      (rare, pure Freeze)
    ]
      ++ if n == 0
        then []
        else
          [ (half, ReadAt <$> iGen),
            (half, WriteAt <$> iGen <*> valGen),
            (half, ExchangeAt <$> iGen <*> valGen)
          ]
  where
    rare = 1
    often = 10
    half = 5
    keyGen = QC.chooseInt (-30, 30)
    keyGen2 = do
      l <- QC.chooseInt (-30, 30)
      r <- QC.chooseInt (l, 30)
      pure (l, r)
    -- use non-negative values for monotoniously increasing sum
    valGen = Sum <$> QC.chooseInt (0, 10)
    -- NOTE: it might throw an error on overflow:
    fGen = Affine1.new <$> QC.chooseInt (0, 4) <*> QC.chooseInt (0, 4)
    iGen = QC.chooseInt (0, n - 1)
    maybeIGen = QC.chooseInt (-1, n)

-- | containers. (referencial implementation)
handleRef :: Int -> M.Map Int (Sum Int) -> Query -> (M.Map Int (Sum Int), Result)
handleRef capacity m q = case q of
  Reset -> (M.empty, None)
  Capacity -> (m, I capacity)
  Size -> (m, I (M.size m))
  Member k -> (m, B (M.member k m))
  Adjust k v -> (M.adjust (+ v) k m, None)
  Insert k v -> (M.insert k v m, None)
  InsertWith k v -> (M.insertWith (+) k v m, None)
  Delete k -> (M.delete k m, MS $ M.lookup k m)
  Delete_ k -> (M.delete k m, None)
  Prod (!l, !r) -> (m, S $ prod l r)
  ProdMaybe (!l, !r)
    | l > r -> (m, MS Nothing)
    | otherwise -> (m, MS . Just $ prod l r)
  ProdAll -> (m, S $ M.foldl' (<>) mempty m)
  ApplyIn (!l, !r) f -> (apply l r f, None)
  ApplyAll f -> (M.map (segAct f) m, None)
  LookupLE k -> (m, MKV $ M.lookupLE k m)
  LookupLT k -> (m, MKV $ M.lookupLT k m)
  LookupGE k -> (m, MKV $ M.lookupGE k m)
  LookupGT k -> (m, MKV $ M.lookupGT k m)
  ReadAt i -> (m, S . snd $ M.elemAt i m)
  ReadMaybeAt i
    | 0 <= i && i < M.size m -> (m, MS $ snd <$> M.lookupMin (M.drop i m))
    | otherwise -> (m, MS Nothing)
  WriteAt i x -> (M.updateAt (\_ _ -> Just x) i m, None)
  ExchangeAt i x -> (M.updateAt (\_ _ -> Just x) i m, S (snd (M.elemAt i m)))
  ProdInInterval (!l, !r) -> (m, S (prodIn l r))
  ApplyInInterval (!l, !r) f -> (applyIn l r f, None)
  ILowerBoundProd v -> (m, lbProd v)
  ILowerBoundProdM v -> (m, lbProd v)
  Freeze -> (m, F (VU.fromList (M.assocs m)))
  where
    slice l r = M.takeWhileAntitone (< r) . M.dropWhileAntitone (< l)
    prod l r = M.foldl' (<>) mempty $ slice l r m
    apply l r f = M.mapWithKey (\k x -> if l <= k && k < r then segAct f x else x) m
    prodIn l r =
      L.foldl' (<>) mempty
        . map snd
        . filter (\(!i, !_) -> l <= i && i < r)
        . zip [0 :: Int ..]
        $ M.elems m
    applyIn l r f =
      M.fromList
        . zipWith
          (\i (!k, !x) -> if l <= i && i < r then (k, segAct f x) else (k, x))
          [0 :: Int ..]
        $ M.assocs m
    -- lb x = I . length . takeWhile (<= x) $ M.elems m
    lbProd x = I . length . takeWhile (<= x) . tail . L.scanl' (<>) mempty $ M.elems m

-- | ac-library-hs.
handleAcl :: (HasCallStack, PrimMonad m) => Map.Map (PrimState m) (Affine1 Int) Int (Sum Int) -> Query -> m Result
handleAcl m q = case q of
  Reset -> do
    Map.reset m
    pure None
  Capacity -> do
    pure . I $ Map.capacity m
  Size -> do
    I <$> Map.size m
  Insert k v -> do
    Map.insert m k v
    pure None
  InsertWith k v -> do
    Map.insertWith m (+) k v
    pure None
  Adjust k v -> do
    Map.adjust m (+ v) k
    pure None
  Delete k -> do
    MS <$> Map.delete m k
  Delete_ k -> do
    Map.delete_ m k
    pure None
  Prod (!l, !r) -> do
    S <$> Map.prod m l r
  ProdMaybe (!l, !r) -> do
    MS <$> Map.prodMaybe m l r
  ProdAll -> do
    S <$> Map.allProd m
  ApplyIn (!l, !r) f -> do
    Map.applyIn m l r f
    pure None
  ApplyAll f -> do
    Map.applyAll m f
    pure None
  Member k -> do
    B <$> Map.member m k
  LookupLE v -> do
    MKV <$> Map.lookupLE m v
  LookupLT v -> do
    MKV <$> Map.lookupLT m v
  LookupGE v -> do
    MKV <$> Map.lookupGE m v
  LookupGT v -> do
    MKV <$> Map.lookupGT m v
  ReadAt i -> do
    S <$> Map.readAt m i
  ReadMaybeAt i -> do
    MS <$> Map.readMaybeAt m i
  WriteAt i x -> do
    Map.writeAt m i x
    pure None
  ExchangeAt i x -> do
    S <$> Map.exchangeAt m i x
  ProdInInterval (!l, !r) -> do
    S <$> Map.prodInInterval m l r
  ApplyInInterval (!l, !r) f -> do
    Map.applyInInterval m l r f
    pure None
  ILowerBoundProd xRef -> do
    I <$> Map.ilowerBoundProd m (\_ x -> x <= xRef)
  ILowerBoundProdM xRef -> do
    I <$> Map.ilowerBoundProdM m (\_ x -> pure (x <= xRef))
  Freeze -> do
    F <$> Map.freeze m

prop_randomTest :: Init -> QC.Property
prop_randomTest Init {..} = QCM.monadicIO $ do
  m <- QCM.run mapM
  foldM_
    ( \ref _ -> do
        query <- QCM.pick (queryGen (M.size ref))
        -- run the query
        let (!ref', !expected) = handleRef q ref query
        res <- QCM.run $ handleAcl m query
        QCM.assertWith (expected == res) $ show (query, expected, res)
        pure ref'
    )
    ref0
    [0 .. q - 1]

prop_build :: [Int] -> QC.Property
prop_build ks_ =
  let ks = VU.fromList ks_
      expected = VU.modify VAI.sort ks
      res = runST $ do
        m <- Map.build @_ @() @Int @() (VU.length ks) $ VU.map (,()) ks
        VU.map fst <$> Map.freeze m
   in expected QC.=== res

tests :: [TestTree]
tests =
  [ QC.testProperty "random test" prop_randomTest,
    QC.testProperty "build" prop_build
  ]
