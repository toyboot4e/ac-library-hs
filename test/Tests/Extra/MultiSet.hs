{-# LANGUAGE RecordWildCards #-}

module Tests.Extra.MultiSet (tests) where

import AtCoder.Extra.MultiSet qualified as MS
import Control.Monad (foldM_, when)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST (RealWorld)
import Data.IntMap qualified as IM
import Data.IntSet qualified as IS
import Data.Vector.Algorithms.Intro qualified as VAI
import Data.Vector.Unboxed qualified as VU
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec
import Test.QuickCheck.Monadic as QCM
import Test.Tasty
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck as QC

spec_invalid :: IO TestTree
spec_invalid = testSpec "capacity limit" $ do
  it "throws error 1" $ do
    ms <- MS.new @_ 1
    MS.inc ms 0
    MS.inc ms 0
    MS.inc ms 1 `shouldThrow` anyException

  it "throws error 2" $ do
    ms <- MS.new @_ 2
    MS.inc ms 0
    MS.inc ms 1
    MS.inc ms 2 `shouldThrow` anyException

  it "throws error 2" $ do
    ms <- MS.new @_ 2
    MS.inc ms 0
    MS.inc ms 1
    MS.delete ms 1
    MS.inc ms 2 `shouldThrow` anyException

data Init = Init
  { n :: {-# UNPACK #-} !Int,
    ref0 :: !(IM.IntMap Int),
    msM :: !(IO (MS.MultiSet RealWorld))
  }

instance Show Init where
  show Init {..} = show (n, ref0)

instance QC.Arbitrary Init where
  arbitrary = do
    n <- QC.chooseInt (1, 10)
    pure $ Init n IM.empty (MS.new n)

data Query
  = Member Int
  | NotMember Int
  | Lookup Int
  | Inc Int
  | -- Safe* are performed to not create negative count
    SafeDec Int
  | SafeAdd Int Int
  | SafeSub Int Int
  | Insert Int Int
  | Delete Int
  deriving (Show)

-- | Arbitrary return type for the `Query` result.
data Result
  = None
  | B Bool
  | I Int
  | M (Maybe Int)
  deriving (Show, Eq)

queryGen :: Int -> QC.Gen Query
queryGen n = do
  QC.oneof
    [ Member <$> keyGen,
      NotMember <$> keyGen,
      Lookup <$> keyGen,
      Inc <$> keyGen,
      SafeDec <$> keyGen,
      SafeAdd <$> keyGen <*> deltaGen,
      SafeSub <$> keyGen <*> deltaGen,
      Insert <$> keyGen <*> insertValGen,
      Delete <$> keyGen
    ]
  where
    keyGen = QC.chooseInt (-n, n)
    deltaGen = QC.chooseInt (-n, n)
    insertValGen = QC.chooseInt (1, n)

-- | containers. (referencial implementation)
handleRef :: IM.IntMap Int -> Query -> (IM.IntMap Int, Result)
handleRef im q = case q of
  Member key -> (im, B $ IM.member key im)
  NotMember key -> (im, B $ IM.notMember key im)
  Lookup key -> (im, M $ IM.lookup key im)
  Inc key -> (IM.insertWith (+) key 1 im, None)
  SafeDec key -> case IM.lookup key im of
    Just 1 -> (IM.delete key im, None)
    Just n -> (IM.insert key (n - 1) im, None)
    Nothing -> (im, None)
  SafeAdd key dx -> case IM.lookup key im of
    Just n | n + dx == 0 -> (IM.delete key im, None)
    Just n | n + dx > 0 -> (IM.insert key (n + dx) im, None)
    Nothing | dx > 0 -> (IM.insert key dx im, None)
    _ -> (im, None)
  SafeSub key dx -> case IM.lookup key im of
    Just n | n - dx == 0 -> (IM.delete key im, None)
    Just n | n - dx > 0 -> (IM.insert key (n - dx) im, None)
    Nothing | -dx > 0 -> (IM.insert key (-dx) im, None)
    _ -> (im, None)
  Insert key val -> (IM.insert key val im, None)
  Delete key -> (IM.delete key im, None)

-- | ac-library-hs.
handleAcl :: (HasCallStack, PrimMonad m) => MS.MultiSet (PrimState m) -> Query -> m Result
handleAcl ms q = case q of
  Member key -> B <$> MS.member ms key
  NotMember key -> B <$> MS.notMember ms key
  Lookup key -> M <$> MS.lookup ms key
  Inc key -> do
    MS.inc ms key
    pure None
  SafeDec key -> do
    b <- MS.member ms key
    when b $ do
      MS.dec ms key
    pure None
  SafeAdd key dx -> do
    MS.lookup ms key >>= \case
      Just n | n + dx >= 0 -> do
        MS.add ms key dx
      Nothing | dx >= 0 -> do
        MS.add ms key dx
      _ -> pure ()
    pure None
  SafeSub key dx -> do
    MS.lookup ms key >>= \case
      Just n | n - dx >= 0 -> do
        MS.sub ms key dx
      Nothing | -dx >= 0 -> do
        MS.sub ms key dx
      _ -> pure ()
    pure None
  Insert key val -> do
    MS.insert ms key val
    pure None
  Delete key -> do
    MS.delete ms key
    pure None

-- | Ensures the capacity limit.
passQuery :: Int -> IS.IntSet -> Query -> Bool
passQuery limit is (Inc k) = IS.member k is || IS.size is < limit
passQuery limit is (SafeAdd k dx) = IS.member k is || IS.size is < limit || dx <= 0
passQuery limit is (SafeSub k dx) = IS.member k is || IS.size is < limit || dx >= 0
passQuery limit is (Insert k _) = IS.member k is || IS.size is < limit
passQuery _ _ _ = True

-- | Records used keys.
recordKey :: IS.IntSet -> Query -> IS.IntSet
recordKey is (Inc k) = IS.insert k is
recordKey is (SafeAdd k dx) | dx > 0 = IS.insert k is
recordKey is (SafeSub k dx) | dx < 0 = IS.insert k is
recordKey is (Insert k _) = IS.insert k is
recordKey is _ = is

-- TODO: record used key

prop_randomTest :: Init -> QC.Property
prop_randomTest Init {..} = QCM.monadicIO $ do
  ms <- QCM.run msM
  q <- QCM.pick $ QC.chooseInt (1, 5 * n)
  qs <- QCM.pick $ QC.vectorOf q (queryGen n)

  foldM_
    ( \(!ref, !keys) query -> do
        if passQuery n keys query
          then do
            -- run the query
            let (!ref', !expected) = handleRef ref query
            res <- QCM.run $ handleAcl ms query
            QCM.assertWith (expected == res) $ show (query, expected, res)

            -- check the map contents:
            let assocsE = VU.modify VAI.sort (VU.fromList (IM.assocs ref'))
            assocs <- QCM.run $ VU.modify VAI.sort <$> MS.unsafeAssocs ms
            QCM.assertWith (assocsE == assocs) $ show ("- assocs", assocsE, assocs)

            let keysE = VU.modify VAI.sort (VU.fromList (IM.keys ref'))
            keys_ <- QCM.run $ VU.modify VAI.sort <$> MS.unsafeKeys ms
            QCM.assertWith (keysE == keys_) $ show ("- keys", keysE, keys_)

            let elemsE = VU.modify VAI.sort (VU.fromList (IM.elems ref'))
            elems <- QCM.run $ VU.modify VAI.sort <$> MS.unsafeElems ms
            QCM.assertWith (elemsE == elems) $ show ("- elems", elemsE, elems)

            let sizeE = IM.size ref'
            size <- QCM.run $ MS.size ms
            QCM.assertWith (sizeE == size) $ show ("- size", sizeE, size)

            pure (ref', recordKey keys query)
          else
            pure (ref, keys)
    )
    (ref0, IS.empty)
    qs

-- TODO: test invariant

tests :: [TestTree]
tests =
  [ unsafePerformIO spec_invalid,
    QC.testProperty "random test" prop_randomTest
  ]
