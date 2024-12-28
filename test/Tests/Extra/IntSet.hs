{-# LANGUAGE RecordWildCards #-}

module Tests.Extra.IntSet where

import AtCoder.Extra.IntSet qualified as IS
import Control.Monad (foldM_)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST (RealWorld)
import Data.Set qualified as ISR -- R: referencial implementation
import Data.Vector.Unboxed qualified as VU
import GHC.Stack (HasCallStack)
import Test.QuickCheck.Monadic as QCM
import Test.Tasty
import Test.Tasty.QuickCheck as QC

data Init = Init
  { n :: {-# UNPACK #-} !Int,
    ref0 :: !(ISR.Set Int),
    isM :: !(IO (IS.IntSet RealWorld))
  }

instance Show Init where
  show Init {..} = show (n, ref0)

instance QC.Arbitrary Init where
  arbitrary = do
    n <- QC.chooseInt (1, 10)
    pure $ Init n ISR.empty (IS.new n)

data Query
  = Member Int
  | NotMember Int
  | Insert Int
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
  deriving (Show, Eq)

queryGen :: Int -> QC.Gen Query
queryGen n = do
  QC.oneof
    [ Member <$> lookupKeyGen,
      NotMember <$> lookupKeyGen,
      -- insert is partial function
      Insert <$> insertKeyGen,
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

-- | containers. (referencial implementation)
handleRef :: ISR.Set Int -> Query -> (ISR.Set Int, Result)
handleRef is q = case q of
  Member k -> (is, B $ ISR.member k is)
  NotMember k -> (is, B . not $ ISR.member k is)
  Insert k -> (ISR.insert k is, None)
  Delete k -> (ISR.delete k is, B $ ISR.member k is)
  Delete_ k -> (ISR.delete k is, None)
  LookupGE k -> (is, M (ISR.lookupGE k is))
  LookupGT k -> (is, M (ISR.lookupGT k is))
  LookupLE k -> (is, M (ISR.lookupLE k is))
  LookupLT k -> (is, M (ISR.lookupLT k is))
  LookupMin -> (is, M (ISR.lookupMin is))
  LookupMax -> (is, M (ISR.lookupMax is))
  DeleteMin -> wrapK ISR.deleteFindMin
  DeleteMax -> wrapK ISR.deleteFindMax
  where
    wrapK f
      | ISR.null is = (is, M Nothing)
      | otherwise = let (!kv, !is') = f is in (is', M (Just kv))

-- | ac-library-hs.
handleAcl :: (HasCallStack, PrimMonad m) => IS.IntSet (PrimState m) -> Query -> m Result
handleAcl is q = case q of
  Member k -> B <$> IS.member is k
  NotMember k -> B <$> IS.notMember is k
  Insert k -> do
    IS.insert is k
    pure None
  Delete k -> B <$> IS.delete is k
  Delete_ k -> do
    IS.delete_ is k
    pure None
  LookupGE k -> M <$> IS.lookupGE is k
  LookupGT k -> M <$> IS.lookupGT is k
  LookupLE k -> M <$> IS.lookupLE is k
  LookupLT k -> M <$> IS.lookupLT is k
  LookupMin -> M <$> IS.lookupMin is
  LookupMax -> M <$> IS.lookupMax is
  DeleteMin -> M <$> IS.deleteMin is
  DeleteMax -> M <$> IS.deleteMax is

prop_randomTest :: Init -> QC.Property
prop_randomTest Init {..} = QCM.monadicIO $ do
  is <- QCM.run isM
  q <- QCM.pick $ QC.chooseInt (1, 5 * n)
  qs <- QCM.pick $ QC.vectorOf q (queryGen n)
  foldM_
    ( \ref query -> do
        let (!ref', !expected) = handleRef ref query
        res <- QCM.run $ handleAcl is query
        QCM.assertWith (expected == res) $ show (query, expected, res)

        -- check the set contents:
        let keysE = VU.fromList $ ISR.elems ref'
        keys <- QCM.run $ IS.keys is
        QCM.assertWith (keysE == keys) $ show ("- keys", keysE, keys)

        let sizeE = ISR.size ref'
        size <- QCM.run $ IS.size is
        QCM.assertWith (sizeE == size) $ show ("- size", sizeE, size)

        pure ref'
    )
    ref0
    qs

tests :: [TestTree]
tests =
  [ QC.testProperty "randomTest" prop_randomTest
  ]
