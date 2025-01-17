{-# LANGUAGE RecordWildCards #-}

module Tests.Extra.HashMap where

import AtCoder.Extra.HashMap qualified as HM
import Control.Monad (foldM_)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST (RealWorld)
import Data.HashMap.Strict qualified as HMR -- R: referencial implementation
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
    hm <- HM.new @_ @Int 1
    HM.insert hm 0 0
    HM.insert hm 0 1
    HM.insert hm 1 2 `shouldThrow` anyException

  it "throws error 2" $ do
    hm <- HM.new @_ @Int 2
    HM.insert hm 0 0
    HM.insert hm 1 1
    HM.insert hm 1 2
    HM.insert hm 2 2 `shouldThrow` anyException

data Init = Init
  { capacity :: {-# UNPACK #-} !Int,
    ref0 :: !(HMR.HashMap Int Int),
    hmM :: !(IO (HM.HashMap RealWorld Int))
  }

instance Show Init where
  show Init {..} = show (capacity, ref0)

instance QC.Arbitrary Init where
  arbitrary = do
    capacity <- QC.chooseInt (1, 10)
    pure $ Init capacity HMR.empty (HM.new capacity)

data Query
  = Size
  | Member Int
  | NotMember Int
  | Lookup Int
  | Insert Int Int
  | InsertWithAdd Int Int
  | Exchange Int Int
  | ModifyAdd Int Int
  | Clear
  deriving (Show)

instance QC.Arbitrary Query where
  arbitrary = do
    QC.frequency
      [ (rare, pure Clear),
        (often, pure Size),
        (often, Member <$> keyGen),
        (often, NotMember <$> keyGen),
        (often, Lookup <$> keyGen),
        (often, Insert <$> keyGen <*> valGen),
        (often, InsertWithAdd <$> keyGen <*> valGen),
        (often, Exchange <$> keyGen <*> valGen),
        (often, ModifyAdd <$> keyGen <*> valGen)
      ]
    where
      rare = 1
      often = 10
      keyGen = QC.chooseInt (-5, 5)
      valGen = QC.arbitrary @Int

-- | Arbitrary return type for the `Query` result.
data Result
  = None
  | B Bool
  | I Int
  | M (Maybe Int)
  deriving (Show, Eq)

-- | containers. (referencial implementation)
handleRef :: HMR.HashMap Int Int -> Query -> (HMR.HashMap Int Int, Result)
handleRef hm q = case q of
  Size -> (hm, I $ HMR.size hm)
  Member k -> (hm, B $ HMR.member k hm)
  NotMember k -> (hm, B . not $ HMR.member k hm)
  Lookup k -> (hm, M $ HMR.lookup k hm)
  Insert k v -> (HMR.insert k v hm, None)
  InsertWithAdd k v -> (HMR.insertWith (+) k v hm, None)
  Exchange k v -> (HMR.insert k v hm, M $ HMR.lookup k hm)
  ModifyAdd k v -> (HMR.adjust (+ v) k hm, None)
  -- Delete k -> (HMR.delete k hm, HMR.lookup k hm)
  Clear -> (HMR.empty, None)

-- | ac-library-hs.
handleAcl :: (PrimMonad m) => HM.HashMap (PrimState m) Int -> Query -> m Result
handleAcl hm q = case q of
  Size -> I <$> HM.size hm
  Member k -> B <$> HM.member hm k
  NotMember k -> B <$> HM.notMember hm k
  Lookup k -> M <$> HM.lookup hm k
  Insert k v -> do
    HM.insert hm k v
    pure None
  InsertWithAdd k v -> do
    HM.insertWith hm (+) k v
    pure None
  Exchange k v -> M <$> HM.exchange hm k v
  ModifyAdd k v -> do
    HM.modify hm (+ v) k
    pure None
  -- Delete k -> HM.delete hm k
  Clear -> do
    HM.clear hm
    pure None

-- | Ensures the capacity limit.
passQuery :: Int -> HMR.HashMap Int Int -> Query -> Bool
passQuery limit is (Insert k _) = HMR.member k is || HMR.size is < limit
passQuery limit is (InsertWithAdd k _) = HMR.member k is || HMR.size is < limit
passQuery limit is (Exchange k _) = HMR.member k is || HMR.size is < limit
passQuery _ _ _ = True

prop_randomTest :: Init -> QC.Property
prop_randomTest Init {..} = QCM.monadicIO $ do
  hm <- QCM.run hmM
  q <- QCM.pick $ QC.chooseInt (1, 5 * capacity)
  qs <- QCM.pick $ QC.vectorOf q (QC.arbitrary @Query)
  foldM_
    ( \ref query -> do
        if passQuery capacity ref query
          then do
            let (!ref', !expected) = handleRef ref query
            res <- QCM.run $ handleAcl hm query
            QCM.assertWith (expected == res) $ show (query, expected, res)

            -- check the map contents:
            let assocsE = VU.modify (VAI.sortBy compare) . VU.fromList $ HMR.toList ref'
            assocs <- QCM.run $ VU.modify (VAI.sortBy compare) <$> HM.unsafeAssocs hm
            QCM.assertWith (assocsE == assocs) $ show ("- assocs", assocsE, assocs)

            let keysE = VU.modify (VAI.sortBy compare) . VU.fromList $ HMR.keys ref'
            keys <- QCM.run $ VU.modify (VAI.sortBy compare) <$> HM.unsafeKeys hm
            QCM.assertWith (keysE == keys) $ show ("- keys", keysE, keys)

            let elemsE = VU.modify (VAI.sortBy compare) . VU.fromList $ HMR.elems ref'
            elems <- QCM.run $ VU.modify (VAI.sortBy compare) <$> HM.unsafeElems hm
            QCM.assertWith (elemsE == elems) $ show ("- elems", elemsE, elems)

            let sizeE = HMR.size ref'
            size <- QCM.run $ HM.size hm
            QCM.assertWith (sizeE == size) $ show ("- size", sizeE, size)

            pure ref'
          else pure ref
    )
    ref0
    qs

tests :: [TestTree]
tests =
  [ unsafePerformIO spec_invalid,
    QC.testProperty "random test" prop_randomTest
  ]
