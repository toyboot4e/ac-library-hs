{-# LANGUAGE RecordWildCards #-}

module Tests.Extra.IntMap where

import AtCoder.Extra.IntMap qualified as IM
import Control.Monad (foldM_)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST (RealWorld)
import Data.Map.Strict qualified as IMR -- R: referencial implementation
import Data.Vector.Unboxed qualified as VU
import GHC.Stack (HasCallStack)
import Test.QuickCheck.Monadic as QCM
import Test.Tasty
import Test.Tasty.QuickCheck as QC

data Init = Init
  { n :: {-# UNPACK #-} !Int,
    ref0 :: !(IMR.Map Int Int),
    imM :: !(IO (IM.IntMap RealWorld Int))
  }

instance Show Init where
  show Init {..} = show (n, ref0)

instance QC.Arbitrary Init where
  arbitrary = do
    n <- QC.chooseInt (1, 10)
    pure $ Init n IMR.empty (IM.new n)

data Query
  = Size
  | Member Int
  | NotMember Int
  | Lookup Int
  | Insert Int Int
  | InsertWithAdd Int Int
  | Delete Int
  | Delete_ Int
  | LookupGE Int
  | LookupGT Int
  | LookupLE Int
  | LookupLT Int
  | LookupMin
  | LookupMax
  | DeleteMin
  | DeleteMax
  deriving (Show)

-- | Arbitrary return type for the `Query` result.
data Result
  = None
  | B Bool
  | I Int
  | M (Maybe Int)
  | KV (Maybe (Int, Int))
  | XS (VU.Vector Int)
  | KVS (VU.Vector (Int, Int))
  deriving (Show, Eq)

queryGen :: Int -> QC.Gen Query
queryGen n = do
  QC.oneof
    [ pure Size,
      Member <$> lookupKeyGen,
      NotMember <$> lookupKeyGen,
      Lookup <$> lookupKeyGen,
      -- insert is partial function
      Insert <$> insertKeyGen <*> valGen,
      InsertWithAdd <$> insertKeyGen <*> valGen,
      Delete <$> lookupKeyGen,
      Delete_ <$> lookupKeyGen,
      LookupGE <$> lookupKeyGen,
      LookupGT <$> lookupKeyGen,
      LookupLE <$> lookupKeyGen,
      LookupLT <$> lookupKeyGen,
      pure LookupMin,
      pure LookupMax
    ]
  where
    -- for total functions
    lookupKeyGen = QC.chooseInt (-1, n)
    -- for partial functions
    insertKeyGen = QC.chooseInt (0, n - 1)
    valGen = QC.chooseInt (-10, 10)

-- | containers. (referencial implementation)
handleRef :: IMR.Map Int Int -> Query -> (IMR.Map Int Int, Result)
handleRef im q = case q of
  Size -> (im, I $ IMR.size im)
  Member k -> (im, B $ IMR.member k im)
  NotMember k -> (im, B . not $ IMR.member k im)
  Lookup k -> (im, M $ IMR.lookup k im)
  Insert k v -> (IMR.insert k v im, None)
  InsertWithAdd k v -> (IMR.insertWith (+) k v im, None)
  Delete k -> (IMR.delete k im, B $ IMR.member k im)
  Delete_ k -> (IMR.delete k im, None)
  LookupGE k -> (im, KV (IMR.lookupGE k im))
  LookupGT k -> (im, KV (IMR.lookupGT k im))
  LookupLE k -> (im, KV (IMR.lookupLE k im))
  LookupLT k -> (im, KV (IMR.lookupLT k im))
  LookupMin -> (im, KV (IMR.lookupMin im))
  LookupMax -> (im, KV (IMR.lookupMax im))
  DeleteMin -> wrapKV IMR.deleteFindMin
  DeleteMax -> wrapKV IMR.deleteFindMax
  where
    wrapKV f
      | IMR.null im = (im, KV Nothing)
      | otherwise = let (!kv, !im') = f im in (im', KV (Just kv))

-- | ac-library-hs.
handleAcl :: (HasCallStack, PrimMonad m) => IM.IntMap (PrimState m) Int -> Query -> m Result
handleAcl im q = case q of
  Size -> I <$> IM.size im
  Member k -> B <$> IM.member im k
  NotMember k -> B <$> IM.notMember im k
  Lookup k -> M <$> IM.lookup im k
  Insert k v -> do
    IM.insert im k v
    pure None
  InsertWithAdd k v -> do
    IM.insertWith im (+) k v
    pure None
  Delete k -> B <$> IM.delete im k
  Delete_ k -> do
    IM.delete_ im k
    pure None
  LookupGE k -> KV <$> IM.lookupGE im k
  LookupGT k -> KV <$> IM.lookupGT im k
  LookupLE k -> KV <$> IM.lookupLE im k
  LookupLT k -> KV <$> IM.lookupLT im k
  LookupMin -> KV <$> IM.lookupMin im
  LookupMax -> KV <$> IM.lookupMax im
  DeleteMin -> KV <$> IM.deleteMin im
  DeleteMax -> KV <$> IM.deleteMax im

prop_randomTest :: Init -> QC.Property
prop_randomTest Init {..} = QCM.monadicIO $ do
  im <- QCM.run imM
  q <- QCM.pick $ QC.chooseInt (1, 5 * n)
  qs <- QCM.pick $ QC.vectorOf q (queryGen n)
  foldM_
    ( \ref query -> do
        let (!ref', !expected) = handleRef ref query
        res <- QCM.run $ handleAcl im query
        QCM.assertWith (expected == res) $ show (query, expected, res)

        -- check the map contents:
        let assocsE = VU.fromList $ IMR.assocs ref'
        assocs <- QCM.run $ IM.assocs im
        QCM.assertWith (assocsE == assocs) $ show ("- assocs", assocsE, assocs)

        let keysE = VU.fromList $ IMR.keys ref'
        keys <- QCM.run $ IM.keys im
        QCM.assertWith (keysE == keys) $ show ("- keys", keysE, keys)

        let elemsE = VU.fromList $ IMR.elems ref'
        elems <- QCM.run $ IM.elems im
        QCM.assertWith (elemsE == elems) $ show ("- elems", elemsE, elems)

        let sizeE = IMR.size ref'
        size <- QCM.run $ IM.size im
        QCM.assertWith (sizeE == size) $ show ("- size", sizeE, size)

        pure ref'
    )
    ref0
    qs

tests :: [TestTree]
tests =
  [ QC.testProperty "randomTest" prop_randomTest
  ]
