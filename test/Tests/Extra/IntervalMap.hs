{-# LANGUAGE RecordWildCards #-}

module Tests.Extra.IntervalMap where

import AtCoder.Extra.IntervalMap qualified as ITM
import AtCoder.Internal.Buffer qualified as ACIB
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST (RealWorld, runST)
import Data.Foldable (for_)
import Data.List qualified as L
import Data.Map qualified as M
import Data.Primitive.MutVar
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import Test.QuickCheck.Monadic as QCM
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

-- | buildM should call onAdd and onDel correctly
prop_buildM :: QC.Gen QC.Property
prop_buildM = do
  n <- QC.chooseInt (1, 10)
  xs <- QC.vectorOf n (QC.chooseInt (-1, 1))

  let groups = VU.group $ VU.fromList xs
  let lens = L.scanl' (+) (0 :: Int) $ map VU.length groups
  let expected = VU.fromList $ L.zip3 lens (tail lens) (L.map VU.head groups)

  let res = runST $ do
        buf <- ACIB.new @_ @(Int, Int, Int) n
        _ <- ITM.buildM (VU.fromList xs) $ \l r x -> do
          ACIB.pushBack buf (l, r, x)
        ACIB.freeze buf

  pure $ res QC.=== expected

data Init = Init
  { n :: {-# UNPACK #-} !Int,
    refM :: !(IO (VUM.MVector RealWorld Int)),
    itmM :: !(IO (ITM.IntervalMap RealWorld Int))
  }

instance Show Init where
  show Init {..} = show ("Init", n)

instance QC.Arbitrary Init where
  arbitrary = do
    n <- QC.chooseInt (1, 20)
    pure $ Init n (VUM.replicate n undef) (ITM.new n)

data Query
  = Contains Int
  | Intersects (Int, Int)
  | Lookup (Int, Int)
  | -- | Read (Int, Int)
    ReadMaybe (Int, Int)
  | Insert (Int, Int) Int
  | Delete (Int, Int)
  | Overwrite (Int, Int) Int
  | Freeze
  | TestFreq
  deriving (Show)

-- | Arbitrary return type for the `Query` result.
data Result
  = None
  | B Bool
  | I Int
  | LRX (Maybe (Int, Int, Int))
  | M (Maybe Int)
  | XS (VU.Vector (Int, (Int, Int)))
  | -- | Counts (len * (len + 1) / 2) for each interval and sum them up.
    Freq (M.Map Int Int)
  deriving (Show, Eq)

queryGen :: Int -> QC.Gen Query
queryGen n = do
  QC.oneof
    [ Contains <$> keyGen,
      Intersects <$> intervalGen,
      Lookup <$> intervalGen,
      ReadMaybe <$> intervalGen,
      Insert <$> intervalGen <*> valGen,
      Delete <$> intervalGen,
      Overwrite <$> intervalGen <*> valGen,
      pure Freeze
      -- TestFreq is manually given
      -- pure TestFreq
    ]
  where
    keyGen = QC.chooseInt (0, n - 1)
    valGen = QC.chooseInt (-20, 20)
    -- half-open interval
    intervalGen = do
      l <- QC.chooseInt (0, n)
      r <- QC.chooseInt (l, n)
      pure (l, r)

undef :: Int
undef = minBound `div` 2

-- | Half-open intervals.
toIntervals :: (PrimMonad m) => VUM.MVector (PrimState m) Int -> m (VU.Vector (Int, Int, Int))
toIntervals vec = do
  vec' <- VU.freeze vec
  let groups = VU.group vec'
  let lens = L.scanl' (+) (0 :: Int) $ map VU.length groups
  let intervals = zipWith (\xs l -> (l, l + VU.length xs, VU.head xs)) groups lens
  pure . VU.fromList $ filter (\(!_, !_, !x) -> x /= undef) intervals

-- | containers. (referencial implementation)
handleRef :: (PrimMonad m) => VUM.MVector (PrimState m) Int -> Query -> m Result
handleRef vec q = do
  intervals <- toIntervals vec
  case q of
    Contains i -> do
      pure . B $ VU.any (\(!l, !r, !_) -> l <= i && i < r) intervals
    Intersects (!l, !r)
      | l >= r -> pure $ B False
      | otherwise -> pure . B $ VU.any (\(!l', !r', !_) -> l' <= l && r <= r') intervals
    Lookup (!l, !r)
      | l >= r -> pure $ LRX Nothing
      | otherwise -> pure . LRX $ VU.find (\(!l', !r', !_) -> l' <= l && r <= r') intervals
    ReadMaybe (!l, !r)
      | l >= r -> pure $ M Nothing
      | otherwise -> pure $ maybe (M Nothing) (M . Just . (\(!_, !_, !x) -> x)) $ VU.find (\(!l', !r', !_) -> l' <= l && r <= r') intervals
    Insert (!l, !r) x -> do
      for_ [l .. r - 1] $ \i -> do
        VGM.write vec i x
      pure None
    Delete (!l, !r) -> do
      for_ [l .. r - 1] $ \i -> do
        VGM.write vec i undef
      pure None
    Overwrite (!l, !r) x
      | l >= r -> pure None
      | otherwise -> do
          let interval = VU.find (\(!l', !r', !_) -> l' <= l && r <= r') intervals
          case interval of
            Just (!l', !r', !_) -> do
              for_ [l' .. r' - 1] $ \i -> do
                VGM.write vec i x
            _ -> pure ()
          pure None
    Freeze -> pure . XS $ VU.map (\(!l, !r, !x) -> (l, (r, x))) intervals
    TestFreq -> pure . Freq . M.fromListWith (+) . VU.toList $ VU.map (\(!l, !r, !x) -> (x, (r - l) * ((r - l) + 1) `div` 2)) intervals

-- | ac-library-hs.
handleAcl :: (HasCallStack, PrimMonad m) => MutVar (PrimState m) (M.Map Int Int) -> ITM.IntervalMap (PrimState m) Int -> Query -> m Result
handleAcl freq itm q = case q of
  Contains i -> do
    B <$> ITM.contains itm i
  Intersects (!l, !r) -> do
    B <$> ITM.intersects itm l r
  Lookup (!l, !r) -> do
    LRX <$> ITM.lookup itm l r
  ReadMaybe (!l, !r) -> do
    M <$> ITM.readMaybe itm l r
  Insert (!l, !r) x -> do
    ITM.insertM itm l r x onAdd onDel
    pure None
  Delete (!l, !r) -> do
    ITM.deleteM itm l r onAdd onDel
    pure None
  Overwrite (!l, !r) x -> do
    ITM.overwriteM itm l r x onAdd onDel
    pure None
  Freeze -> do
    XS . VU.map (\(!l, (!r, !x)) -> (l, (r, x))) <$> ITM.freeze itm
  TestFreq -> Freq <$> readMutVar freq
  where
    onAdd l r x = do
      let len = r - l
      let delta = len * (len + 1) `div` 2
      modifyMutVar freq $ M.insertWith (+) x delta
    onDel l r x = modifyMutVar freq $ \m -> do
      let len = r - l
      let delta = len * (len + 1) `div` 2
      case M.lookup x m of
        Just n
          | n - delta == 0 -> M.delete x m
          | otherwise -> M.insert x (n - delta) m
        Nothing -> M.insert x delta m

prop_randomTest :: Init -> QC.Property
prop_randomTest Init {..} = QCM.monadicIO $ do
  ref <- QCM.run refM
  im <- QCM.run itmM
  freq <- QCM.run $ newMutVar M.empty
  q <- QCM.pick $ QC.chooseInt (1, 10 * n)
  qs <- QCM.pick $ QC.vectorOf q (queryGen n)
  for_ qs $ \query -> do
    expected <- QCM.run $ handleRef ref query
    res <- QCM.run $ handleAcl freq im query
    QCM.assertWith (expected == res) $ show (query, expected, res)

    -- always test Freq as an invariant
    expectedI <- QCM.run $ handleRef ref Freeze
    resI <- QCM.run $ handleAcl freq im Freeze
    QCM.assertWith (expectedI == resI) $ show (Freeze, expectedI, resI)

    -- always test Freq as an invariant
    expectedF <- QCM.run $ handleRef ref TestFreq
    resF <- QCM.run $ handleAcl freq im TestFreq
    QCM.assertWith (expectedF == resF) $ show (TestFreq, expectedF, resF)

tests :: [TestTree]
tests =
  [ QC.testProperty "buildM" prop_buildM,
    QC.testProperty "randomTest" prop_randomTest
  ]
