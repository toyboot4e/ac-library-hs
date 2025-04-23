{-# LANGUAGE RecordWildCards #-}

module Tests.Extra.Seq (tests) where

import AtCoder.Extra.Monoid.Affine1 (Affine1 (..))
import AtCoder.Extra.Monoid.Affine1 qualified as Affine1
import AtCoder.Extra.Pool qualified as P
import AtCoder.Extra.Seq qualified as Seq
import AtCoder.Internal.Assert qualified as ACIA
import Control.Monad (foldM_, when)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST (RealWorld, runST)
import Data.Foldable (toList)
import Data.List qualified as L
import Data.Semigroup (Sum (..))
import Data.Sequence qualified as S
import Data.Vector.Algorithms.Intro qualified as VAI
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

unit_empty :: TestTree
unit_empty = testCase "empty sequence operations" $ do
  let n = 4 -- TODO: randomize
  seq <- Seq.new @_ @() @() n
  h <- Seq.newNode seq ()
  h1 <- Seq.newSeq seq VU.empty
  h2 <- Seq.newSeq seq VU.empty
  h3 <- Seq.newSeq seq VU.empty
  h4 <- Seq.newSeq seq VU.empty

  -- merge null and null
  Seq.merge seq h1 h2
  assertBool "" . P.nullIndex =<< VGM.read (Seq.unHandle h1) 0
  assertBool "" . P.nullIndex =<< VGM.read (Seq.unHandle h2) 0

  Seq.merge3 seq h1 h2 h3
  assertBool "" . P.nullIndex =<< VGM.read (Seq.unHandle h1) 0
  assertBool "" . P.nullIndex =<< VGM.read (Seq.unHandle h2) 0
  assertBool "" . P.nullIndex =<< VGM.read (Seq.unHandle h3) 0

  Seq.merge4 seq h1 h2 h3 h4
  assertBool "" . P.nullIndex =<< VGM.read (Seq.unHandle h1) 0
  assertBool "" . P.nullIndex =<< VGM.read (Seq.unHandle h2) 0
  assertBool "" . P.nullIndex =<< VGM.read (Seq.unHandle h3) 0
  assertBool "" . P.nullIndex =<< VGM.read (Seq.unHandle h4) 0

  -- merge a sequence and null
  Seq.merge seq h1 h2
  assertBool "" . (== P.Index 0) =<< VGM.read (Seq.unHandle h) 0

  Seq.merge3 seq h1 h2 h3
  assertBool "" . (== P.Index 0) =<< VGM.read (Seq.unHandle h) 0

  Seq.merge4 seq h1 h2 h3 h4
  assertBool "" . (== P.Index 0) =<< VGM.read (Seq.unHandle h) 0

spec_boundaries :: IO TestTree
spec_boundaries = testSpec "boundaries" $ do
  let n = 12 -- TODO: randomize
  seq <- runIO $ Seq.new @_ @() @() n
  h0 <- runIO $ Seq.newSeq seq VU.empty

  describe "split null at zero" $ do
    r1 <- runIO $ Seq.split seq h0 0
    it "split - l" $ (`shouldSatisfy` P.nullIndex) =<< VGM.read (Seq.unHandle h0) 0
    it "split - r" $ (`shouldSatisfy` P.nullIndex) =<< VGM.read (Seq.unHandle r1) 0
    (!r2, !r3) <- runIO $ Seq.split3 seq h0 0 0
    it "split3 - l" $ (`shouldSatisfy` P.nullIndex) =<< VGM.read (Seq.unHandle h0) 0
    it "split3 - m" $ (`shouldSatisfy` P.nullIndex) =<< VGM.read (Seq.unHandle r2) 0
    it "split3 - r" $ (`shouldSatisfy` P.nullIndex) =<< VGM.read (Seq.unHandle r3) 0
    (!r4, !r5, !r6) <- runIO $ Seq.split4 seq h0 0 0 0
    it "split4 - a" $ (`shouldSatisfy` P.nullIndex) =<< VGM.read (Seq.unHandle h0) 0
    it "split4 - b" $ (`shouldSatisfy` P.nullIndex) =<< VGM.read (Seq.unHandle r4) 0
    it "split4 - c" $ (`shouldSatisfy` P.nullIndex) =<< VGM.read (Seq.unHandle r5) 0
    it "split4 - d" $ (`shouldSatisfy` P.nullIndex) =<< VGM.read (Seq.unHandle r6) 0

  describe "split a sequence into a null and some" $ do
    h <- runIO $ Seq.newSeq seq $ VU.replicate 1 ()
    i <- runIO $ VGM.read (Seq.unHandle h) 0
    r <- runIO $ Seq.split seq h 0
    it "a" $ (`shouldSatisfy` P.nullIndex) =<< VGM.read (Seq.unHandle h) 0
    it "b" $ (`shouldSatisfy` (== i)) =<< VGM.read (Seq.unHandle r) 0
    runIO $ Seq.free seq h

  let withSeq len f = do
        h <- Seq.newSeq seq $ VU.replicate len ()
        _ <- f h
        Seq.free seq h
        pure ()

  describe "split bounds (length 1)" $ do
    it "l" $ withSeq 1 $ \h -> (`shouldThrow` anyException) $ Seq.split seq h (-1)
    it "r" $ withSeq 1 $ \h -> (`shouldThrow` anyException) $ Seq.split seq h 2

  -- TODO: assert that h1 == 0
  describe "split bounds (length 2)" $ do
    it "l" $ withSeq 2 $ \h -> (`shouldThrow` anyException) $ Seq.split seq h (-1)
    it "r" $ withSeq 2 $ \h -> (`shouldThrow` anyException) $ Seq.split seq h 3

  pure ()

data Init = Init
  { n :: {-# UNPACK #-} !Int,
    q :: {-# UNPACK #-} !Int,
    ref0 :: !(S.Seq (Sum Int)),
    seqM :: !(IO (Seq.Seq RealWorld (Affine1 Int) (Sum Int), Seq.Handle RealWorld))
  }

instance Show Init where
  show Init {..} = show (n, ref0)

instance QC.Arbitrary Init where
  arbitrary = do
    n <- QC.chooseInt (1, 64)
    q <- QC.chooseInt (1, 5 * n)
    pure $ Init n q (S.replicate n mempty) $ do
      seq <- Seq.new (n + q)
      root <- Seq.newSeq seq (VU.replicate n mempty)
      pure (seq, root)

data Query
  = Reset
  | Read !Int
  | ReadMaybe !Int
  | Write !Int !(Sum Int)
  | Modify !Int !(Sum Int)
  | Exchange !Int !(Sum Int)
  | Prod !(Int, Int)
  | ProdMaybe !(Int, Int)
  | ProdAll
  | ApplyIn !(Int, Int) !(Affine1 Int)
  | ApplyToRoot !(Affine1 Int)
  | -- | Reverse
    Insert !Int !(Sum Int)
  | Delete !Int
  | Delete_ !Int
  | -- | LowerBound (Sum Int)
    LowerBoundProd !(Sum Int)
  | Freeze
  deriving (Show)

-- | Arbitrary return type for the `Query` result.
data Result
  = None
  | B !Bool
  | I !Int
  | S !(Sum Int)
  | MS !(Maybe (Sum Int))
  | F !(VU.Vector (Sum Int))
  deriving (Show, Eq)

queryGen :: Int -> QC.Gen Query
queryGen n = do
  QC.frequency
    [ (rare, pure Reset),
      (half, Read <$> keyGen),
      (half, ReadMaybe <$> maybeKeyGen),
      (often, Write <$> keyGen <*> valGen),
      (often, Modify <$> keyGen <*> valGen),
      (often, Exchange <$> keyGen <*> valGen),
      (half, Prod <$> intervalGen n),
      (half, ProdMaybe <$> maybeIntervalGen),
      (often, pure ProdAll),
      (often, ApplyIn <$> intervalGen n <*> fGen),
      (often, ApplyToRoot <$> fGen),
      (often, Insert <$> QC.chooseInt (0, n) <*> valGen),
      (half, Delete <$> keyGen),
      (half, Delete_ <$> keyGen),
      (often, LowerBoundProd <$> valGen),
      (rare, pure Freeze)
    ]
  where
    rare = 1
    often = 10
    half = 5
    keyGen = QC.chooseInt (0, n - 1)
    maybeKeyGen = QC.chooseInt (-1, n)
    maybeIntervalGen = (,) <$> QC.chooseInt (-1, n + 1) <*> QC.chooseInt (-1, n + 1)
    -- use non-negative values for monotoniously increasing sum
    valGen = Sum <$> QC.chooseInt (0, 10)
    -- NOTE: it might throw an error on overflow:
    fGen = Affine1.new <$> QC.chooseInt (0, 4) <*> QC.chooseInt (0, 4)

-- | containers. (referencial implementation)
handleRef :: S.Seq (Sum Int) -> Query -> (S.Seq (Sum Int), Result)
handleRef seq q = case q of
  Reset -> (S.empty, None)
  Read k -> (seq, S (S.index seq k))
  ReadMaybe k -> (seq, MS (seq S.!? k))
  Write k v -> (S.adjust (const v) k seq, None)
  Modify k dx -> (S.adjust (<> dx) k seq, None)
  Exchange k v -> (S.adjust (const v) k seq, S (S.index seq k))
  Prod (!l, !r) -> (seq, prod l r)
  ProdMaybe (!l, !r)
    | ACIA.testInterval l r n -> (seq, prodMaybe l r)
    | otherwise -> (seq, MS Nothing)
  ProdAll -> (seq, prod 0 (S.length seq))
  ApplyIn (!l, !r) f -> (apply l r f, None)
  ApplyToRoot f -> (apply 0 (S.length seq) f, None)
  Insert k v -> (S.insertAt k v seq, None)
  Delete k -> (S.deleteAt k seq, S (S.index seq k))
  Delete_ k -> (S.deleteAt k seq, None)
  LowerBoundProd x -> (seq, I (ilowerBound x))
  Freeze -> (seq, F (VU.fromList (toList seq)))
  where
    n = S.length seq
    prod l r = S $ L.foldl' (<>) mempty $ map (S.index seq) [l .. r - 1]
    prodMaybe l r = MS . Just $ L.foldl' (<>) mempty $ map (S.index seq) [l .. r - 1]
    apply :: Int -> Int -> Affine1 Int -> S.Seq (Sum Int)
    apply l r f = S.fromList . zipWith g [0 :: Int ..] $ toList seq
      where
        g i x
          | l <= i && i < r = Sum $ Affine1.act f (getSum x)
          | otherwise = x
    ilowerBound x = length . takeWhile (<= x) . tail . L.scanl' (<>) mempty $ toList seq

-- | ac-library-hs.
handleAcl :: (HasCallStack, PrimMonad m) => Seq.Seq (PrimState m) (Affine1 Int) (Sum Int) -> Seq.Handle (PrimState m) -> Query -> m Result
handleAcl seq handle q = case q of
  Reset -> do
    Seq.reset seq
    P.invalidateHandle handle
    pure None
  Read k -> S <$> Seq.read seq handle k
  ReadMaybe k -> MS <$> Seq.readMaybe seq handle k
  Write k v -> do
    Seq.write seq handle k v
    pure None
  Modify k dx -> do
    Seq.modify seq handle (dx <>) k
    pure None
  Exchange k v -> do
    S <$> Seq.exchange seq handle k v
  Prod (!l, !r) -> do
    S <$> Seq.prod seq handle l r
  ProdMaybe (!l, !r) -> do
    MS <$> Seq.prodMaybe seq handle l r
  ProdAll -> do
    S <$> Seq.prodAll seq handle
  ApplyIn (!l, !r) f -> do
    Seq.applyIn seq handle l r f
    pure None
  ApplyToRoot f -> do
    Seq.applyToRoot seq handle f
    pure None
  Insert k v -> do
    Seq.insert seq handle k v
    pure None
  Delete k -> do
    S <$> Seq.delete seq handle k
  Delete_ k -> do
    Seq.delete_ seq handle k
    pure None
  LowerBoundProd x -> I <$> Seq.ilowerBoundProd seq handle (\_ y -> y <= x)
  Freeze -> F <$> Seq.freeze seq handle

-- | Ensures the capacity limit.
filterQuery :: S.Seq (Sum Int) -> Query -> Bool
filterQuery seq q = case q of
  (Read k) -> idx k
  (Write k _) -> idx k
  (Modify k _) -> idx k
  (Exchange k _) -> idx k
  (Prod (!l, !r)) -> itv l r
  (ApplyIn (!l, !r) _) -> itv l r
  (Insert k _) -> 0 <= k && k <= n
  (Delete k) -> idx k
  (Delete_ k) -> idx k
  _ -> True
  where
    n = S.length seq
    idx k = 0 <= k && k < n
    itv l r = 0 <= l && l <= r && r < n

prop_randomTest :: Init -> QC.Property
prop_randomTest Init {..} = QCM.monadicIO $ do
  (!seq, !root) <- QCM.run seqM
  foldM_
    ( \ref _ -> do
        query <- QCM.pick $ do
          if S.null ref
            then -- most operations throw an error for an empty sequence, so insert some value first:
              Insert 0 . Sum <$> QC.chooseInt (0, 10)
            else queryGen $ S.length ref
        if filterQuery ref query
          then do
            -- run the query
            let (!ref', !expected) = handleRef ref query
            res <- QCM.run $ handleAcl seq root query
            QCM.assertWith (expected == res) $ show (query, expected, res)
            pure ref'
          else pure ref
    )
    ref0
    [0 .. q - 1]

prop_bisectIndex :: QC.Gen QC.Property
prop_bisectIndex = do
  n <- QC.chooseInt (1, 64)
  k <- QC.chooseInt (0, n)
  -- The higher order functinos for bisection method must take the index of intereseted node as a
  -- argument
  pure $ runST $ do
    seq <- Seq.new n
    root <- Seq.newSeq @_ @() seq $ VU.generate n Sum
    lastRight1 <- VUM.replicate 1 (0 :: Int)
    i1 <- Seq.ilowerBoundM seq root $ \i _ -> do
      when (i < k) $ do
        VGM.write lastRight1 0 $ i + 1
      pure $ i < k
    lastRight2 <- VUM.replicate 1 (0 :: Int)
    i2 <- Seq.ilowerBoundProdM seq root $ \i _ -> do
      when (i < k) $ do
        VGM.write lastRight2 0 $ i + 1
      pure $ i < k
    i3 <- VGM.read lastRight1 0
    i4 <- VGM.read lastRight2 0
    pure . QC.conjoin $ map (QC.=== k) [i1, i2, i3, i4]

prop_newSeq :: [Int] -> QC.Property
prop_newSeq xs_ = do
  let xs = VU.fromList xs_
      ys = runST $ do
        s <- Seq.new $ VU.length xs
        root <- Seq.newSeq @_ @() @(Sum Int) s $ VU.map Sum xs
        VU.map getSum <$> Seq.freeze s root
   in xs QC.=== ys

prop_lowerBound :: Int -> [Int] -> QC.Property
prop_lowerBound xRef xs_ = do
  let xs = VU.modify VAI.sort $ VU.fromList xs_
      expected = VU.length $ VU.takeWhile (<= xRef) xs
      res = runST $ do
        s <- Seq.new $ VU.length xs
        root <- Seq.newSeq @_ @() @(Sum Int) s $ VU.map Sum xs
        Seq.ilowerBound s root (\_ x -> x <= Sum xRef)
   in expected QC.=== res

tests :: [TestTree]
tests =
  [ unit_empty,
    unsafePerformIO spec_boundaries,
    QC.testProperty "random test" prop_randomTest,
    QC.testProperty "bisect index" prop_bisectIndex,
    QC.testProperty "newSeq preserves the ordering" prop_newSeq,
    QC.testProperty "lowerBound" prop_lowerBound
  ]
