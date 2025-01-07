{-# LANGUAGE RecordWildCards #-}

module Tests.Extra.WaveletMatrix (tests) where

import AtCoder.Extra.WaveletMatrix qualified as WM
import Data.IntMap qualified as IM
import Data.Ord (comparing)
import Data.Vector.Algorithms.Intro qualified as VAI
import Data.Vector.Unboxed qualified as VU
import Test.Hspec
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Tests.Util (intervalGen)

data Init = Init
  { capacity :: {-# UNPACK #-} !Int,
    xs :: !(VU.Vector Int),
    wm :: !WM.WaveletMatrix
  }

instance Show Init where
  show Init {..} = show ("Init", capacity, xs)

instance QC.Arbitrary Init where
  arbitrary = do
    QC.NonNegative n <- QC.arbitrary
    xs <- VU.fromListN n <$> QC.vectorOf n (QC.arbitrary @Int)
    pure $ Init n xs (WM.build xs)

data Query
  = Access !Int
  | KthSmallestIn !(Int, Int) !Int
  | IKthSmallestIn !(Int, Int) !Int
  | KthLargestIn !(Int, Int) !Int
  | IKthLargestIn !(Int, Int) !Int
  | RankBetween !(Int, Int) !(Int, Int)
  | Rank !(Int, Int) !Int
  | Select !Int
  | SelectIn !(Int, Int) !Int
  | SelectKth !Int !Int
  | SelectKthIn !(Int, Int) !Int !Int
  | LookupLE !(Int, Int) !Int
  | LookupLT !(Int, Int) !Int
  | LookupGE !(Int, Int) !Int
  | LookupGT !(Int, Int) !Int
  | AssocsIn !(Int, Int)
  | DescAssocsIn !(Int, Int)
  deriving (Show)

genQuery :: Int -> QC.Gen Query
genQuery n = do
  QC.oneof
    [ Access <$> QC.chooseInt (-1, n),
      KthSmallestIn <$> lr <*> exc,
      IKthSmallestIn <$> lr <*> exc,
      KthLargestIn <$> lr <*> exc,
      IKthLargestIn <$> lr <*> exc,
      RankBetween <$> lr <*> lr,
      Rank <$> lr <*> val,
      Select <$> val,
      SelectIn <$> lr <*> val,
      SelectKth <$> exc <*> val,
      SelectKthIn <$> lr <*> exc <*> val,
      LookupLE <$> lr <*> QC.chooseInt (-1, n),
      LookupLT <$> lr <*> QC.chooseInt (-1, n),
      LookupGE <$> lr <*> QC.chooseInt (-1, n),
      LookupGT <$> lr <*> QC.chooseInt (-1, n),
      AssocsIn <$> lr,
      DescAssocsIn <$> lr
    ]
  where
    exc = QC.chooseInt (0, n)
    lr = intervalGen n
    val = QC.chooseInt (-n, n)

-- | Arbitrary return type for the `Query` result.
data Result
  = I {-# UNPACK #-} !Int
  | M !(Maybe Int)
  | M2 !(Maybe (Int, Int))
  | Assocs [(Int, Int)]
  deriving (Show, Eq)

-- | containers. (referencial implementation)
handleRef :: VU.Vector Int -> Query -> Result
handleRef xs q = case q of
  Access i -> M $ xs VU.!? i
  KthSmallestIn (!l, !r) k -> M $ snd <$> ikthSmallestIn l r k
  IKthSmallestIn (!l, !r) k -> M2 $ ikthSmallestIn l r k
  KthLargestIn (!l, !r) k -> M $ snd <$> ikthLargestIn l r k
  IKthLargestIn (!l, !r) k -> M2 $ ikthLargestIn l r k
  RankBetween (!l, !r) (!xl, !xr) -> rankBetween l r xl xr
  Rank (!l, !r) x -> rankBetween l r x (x + 1)
  Select x -> M $ selectKthIn 0 n 0 x
  SelectIn (!l, !r) x -> M $ selectKthIn l r 0 x
  SelectKth k x -> M $ selectKthIn 0 n k x
  SelectKthIn (!l, !r) k x -> M $ selectKthIn l r k x
  LookupLE (!l, !r) x -> max_ . VU.filter (<= x) . VU.take (r - l) $ VU.drop l xs
  LookupLT (!l, !r) x -> max_ . VU.filter (< x) . VU.take (r - l) $ VU.drop l xs
  LookupGE (!l, !r) x -> min_ . VU.filter (>= x) . VU.take (r - l) $ VU.drop l xs
  LookupGT (!l, !r) x -> min_ . VU.filter (> x) . VU.take (r - l) $ VU.drop l xs
  AssocsIn (!l, !r) -> Assocs . IM.assocs . IM.fromListWith (+) . VU.toList . VU.map (,1) . VU.take (r - l) $ VU.drop l xs
  DescAssocsIn (!l, !r) -> Assocs . reverse . IM.assocs . IM.fromListWith (+) . VU.toList . VU.map (,1) . VU.take (r - l) $ VU.drop l xs
  where
    n = VU.length xs
    max_ ys
      | VU.null ys = M Nothing
      | otherwise = M $ Just $ VU.maximum ys
    min_ ys
      | VU.null ys = M Nothing
      | otherwise = M $ Just $ VU.minimum ys
    ikthSmallestIn l r k =
      (VU.!? k)
        . VU.modify (VAI.sortBy (comparing (\(!i, !x) -> (x, i))))
        . VU.take (r - l)
        . VU.drop l
        . VU.indexed
        $ xs
    ikthLargestIn l r k = ikthSmallestIn l r ((r - l) - (k + 1))
    rankBetween l r xl xr =
      I
        . VU.length
        . VU.filter (\x -> xl <= x && x < xr)
        . VU.take (r - l)
        . VU.drop l
        $ xs
    selectKthIn l r k x =
      (fst <$>)
        . (VU.!? k)
        . VU.filter ((== x) . snd)
        . VU.take (r - l)
        . VU.drop l
        . VU.indexed
        $ xs

handleAcl :: WM.WaveletMatrix -> Query -> Result
handleAcl wm q = case q of
  Access i -> M $ WM.access wm i
  KthSmallestIn (!l, !r) k -> M $ WM.kthSmallestIn wm l r k
  IKthSmallestIn (!l, !r) k -> M2 $ WM.ikthSmallestIn wm l r k
  KthLargestIn (!l, !r) k -> M $ WM.kthLargestIn wm l r k
  IKthLargestIn (!l, !r) k -> M2 $ WM.ikthLargestIn wm l r k
  RankBetween (!l, !r) (!xl, !xr) -> I $ WM.rankBetween wm l r xl xr
  Rank (!l, !r) x -> I $ WM.rankBetween wm l r x (x + 1)
  Select x -> M $ WM.select wm x
  SelectIn (!l, !r) x -> M $ WM.selectIn wm l r x
  SelectKth k x -> M $ WM.selectKth wm k x
  SelectKthIn (!l, !r) k x -> M $ WM.selectKthIn wm l r k x
  LookupLE (!l, !r) x -> M $ WM.lookupLE wm l r x
  LookupLT (!l, !r) x -> M $ WM.lookupLT wm l r x
  LookupGE (!l, !r) x -> M $ WM.lookupGE wm l r x
  LookupGT (!l, !r) x -> M $ WM.lookupGT wm l r x
  AssocsIn (!l, !r) -> Assocs $ WM.assocsIn wm l r
  DescAssocsIn (!l, !r) -> Assocs $ WM.descAssocsIn wm l r

prop_randomTest :: Init -> QC.Gen QC.Property
prop_randomTest Init {..} = do
  qs <- QC.vectorOf capacity (genQuery capacity)
  pure . QC.conjoin $
    map
      ( \q ->
          QC.counterexample (show q) $
            handleRef xs q QC.=== handleAcl wm q
      )
      qs

unit_boundary :: TestTree
unit_boundary = testCase "boundary" $ do
  let n = 5
  let wm = WM.build $ VU.fromList [0, 10, 20, 10, 0]

  let try :: (HasCallStack, Eq a, Show a) => (WM.WaveletMatrix -> Int -> Int -> Int -> Maybe a) -> Int -> IO ()
      try f x = do
        (@?= Nothing) $ f wm (-1) 0 x
        (@?= Nothing) $ f wm 20 (20 + 1) x

  let k = 0
  try WM.kthSmallestIn k
  try WM.ikthSmallestIn k
  try WM.kthLargestIn k
  try WM.ikthLargestIn k

-- try WM.unsafeKthSmallestIn
-- try WM.unsafeIKthSmallestIn
-- try WM.unsafeKthLargestIn
-- try WM.unsafeIKthLargestIn

  let tryRank :: (HasCallStack) => (WM.WaveletMatrix -> Int -> Int -> Int -> Int) -> Int -> IO ()
      tryRank f x = do
        -- out of range
        (@?= 0) $ f wm (-1) 0 x
        (@?= 0) $ f wm n (n + 1) x
        -- reverse
        (@?= 0) $ f wm 1 0 x
        (@?= 0) $ f wm n (n - 1) x
        (@?= 0) $ f wm n 0 x
        -- out of range and reverse
        (@?= 0) $ f wm 0 (-1) x
        (@?= 0) $ f wm (n + 1) n x

  let tryRankBetween :: (HasCallStack) => (WM.WaveletMatrix -> Int -> Int -> Int -> Int -> Int) -> Int -> Int -> IO ()
      tryRankBetween f xl xr = do
        -- out of range
        (@?= 0) $ f wm (-1) 0 xl xr
        (@?= 0) $ f wm n (n + 1) xl xr
        -- reverse
        (@?= 0) $ f wm 1 0 xl xr
        (@?= 0) $ f wm n (n - 1) xl xr
        (@?= 0) $ f wm n 0 xl xr
        -- out of range and reverse
        (@?= 0) $ f wm 0 (-1) xl xr
        (@?= 0) $ f wm (n + 1) n xl xr

  tryRankBetween WM.rankBetween (-1) 1
  tryRank WM.rank 0
  tryRank WM.rank 1
  tryRank WM.rank (-1)

-- TODO: test 

-- (@?= Nothing) $ WM.select wm
-- (@?= Nothing) $ WM.selectKth wm
-- (@?= Nothing) $ WM.selectKthIn wm

-- (@?= Nothing) $ WM.lookupLE wm
-- (@?= Nothing) $ WM.lookupLT wm
-- (@?= Nothing) $ WM.lookupGE wm
-- (@?= Nothing) $ WM.lookupGT wm
-- (@?= Nothing) $ WM.assocsIn wm
-- (@?= Nothing) $ WM.descAssocsIn wm

tests :: [TestTree]
tests =
  [ unit_boundary,
    QC.testProperty "random test" prop_randomTest
  ]
