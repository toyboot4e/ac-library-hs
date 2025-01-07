{-# LANGUAGE RecordWildCards #-}

module Tests.Extra.WaveletMatrix.BitVector where

import AtCoder.Extra.WaveletMatrix.BitVector qualified as BV
import Data.Bit (Bit (..))
import Data.Vector.Unboxed qualified as VU
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Tests.Util (intervalGen)

data Init = Init
  { capacity :: {-# UNPACK #-} !Int,
    ref :: !(VU.Vector Bool),
    bv :: !BV.BitVector
  }
  deriving (Eq, Show)

instance QC.Arbitrary Init where
  arbitrary = do
    QC.Positive capacity <- QC.arbitrary
    bs <- QC.vectorOf capacity (QC.arbitrary @Bool)
    pure $ Init capacity (VU.fromList bs) (BV.build (VU.fromList (map Bit bs)))

data Query
  = Rank0 {-# UNPACK #-} !Int
  | Rank1 {-# UNPACK #-} !Int
  | Select0 {-# UNPACK #-} !Int
  | Select1 {-# UNPACK #-} !Int
  | SelectKth0 {-# UNPACK #-} !(Int, Int) !Int
  | SelectKth1 {-# UNPACK #-} !(Int, Int) !Int
  deriving (Show)

genQuery :: Int -> QC.Gen Query
genQuery n = do
  QC.oneof
    [ Rank0 <$> QC.chooseInt (0, n),
      Rank1 <$> QC.chooseInt (0, n),
      Select0 <$> QC.chooseInt (0, n),
      Select1 <$> QC.chooseInt (0, n),
      SelectKth0 <$> intervalGen n <*> QC.chooseInt (0, n),
      SelectKth1 <$> intervalGen n <*> QC.chooseInt (0, n)
    ]

-- | Arbitrary return type for the `Query` result.
data Result
  = I {-# UNPACK #-} !Int
  | M !(Maybe Int)
  deriving (Show, Eq)

-- | containers. (referencial implementation)
handleRef :: VU.Vector Bool -> Query -> Result
handleRef xs q = case q of
  Rank0 k -> I . VU.length . VU.filter not $ VU.take k xs
  Rank1 k -> I . VU.length . VU.filter id $ VU.take k xs
  Select0 k -> M . (fst <$>) . (VU.!? k) . VU.filter (not . snd) $ VU.indexed xs
  Select1 k -> M . (fst <$>) . (VU.!? k) . VU.filter snd $ VU.indexed xs
  SelectKth0 (!l, !r) k -> M . (fst <$>) . (VU.!? k) . VU.filter (not . snd) . VU.take (r - l) . VU.drop l $ VU.indexed xs
  SelectKth1 (!l, !r) k -> M . (fst <$>) . (VU.!? k) . VU.filter snd . VU.take (r - l) . VU.drop l $ VU.indexed xs

handleAcl :: BV.BitVector -> Query -> Result
handleAcl bv q = case q of
  Rank0 k -> I $ BV.rank0 bv k
  Rank1 k -> I $ BV.rank1 bv k
  Select0 k -> M $ BV.select0 bv k
  Select1 k -> M $ BV.select1 bv k
  SelectKth0 (!l, !r) k -> M $ BV.selectKthIn0 bv l r k
  SelectKth1 (!l, !r) k -> M $ BV.selectKthIn1 bv l r k

prop_randomTest :: Init -> QC.Gen QC.Property
prop_randomTest Init {..} = do
  qs <- QC.vectorOf capacity (genQuery capacity)
  pure . QC.conjoin $
    map
      ( \q ->
          QC.counterexample (show q) $
            handleRef ref q QC.=== handleAcl bv q
      )
      qs

tests :: [TestTree]
tests =
  [ QC.testProperty "random test" prop_randomTest
  ]
