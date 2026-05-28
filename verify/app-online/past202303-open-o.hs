{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

import AtCoder.Extra.SqrtDecomposition qualified as Sd
import Control.Exception
import Control.Monad (when)
import Control.Monad.ST (ST, runST)
import Control.Monad.Trans.State.Strict (StateT (..), evalStateT)
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Char8 qualified as BS
import Data.Foldable (for_)
import Data.Maybe (fromJust, fromMaybe)
import Data.Vector qualified as V
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Mutable qualified as VM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import Debug.Trace
import GHC.Stack (HasCallStack)
import GHC.TypeNats (KnownNat)
import System.Exit (exitSuccess)
import System.IO (stdout)
import System.IO.Error (ioError, userError)
import System.IO.Unsafe (unsafePerformIO)
import Test.QuickCheck qualified as QC
import Util

-- import Util

-- | \(O(n)\) Creates a vector of RLE representation from an input vector.
toRle :: (HasCallStack, Eq a, VU.Unbox a) => VU.Vector a -> VU.Vector (a, Int)
toRle = VU.fromList . map (\xs -> (VU.head xs, VU.length xs)) . VU.group

-- | \(O(ns)\) Creates a vector from an input vector of RLE representation.
unRle :: (HasCallStack, VU.Unbox a) => VU.Vector (a, Int) -> VU.Vector a
unRle = VU.concatMap (\(!a, !n) -> VU.replicate n a)

-- | \(O(n)\) It's lazy: zero length tuples are left to the right
splitAtRle :: forall a. (HasCallStack, VU.Unbox a) => Int -> VU.Vector (a, Int) -> (VU.Vector (a, Int), VU.Vector (a, Int))
splitAtRle 0 rle = (VU.empty, rle)
splitAtRle n rle = inner 0 0
  where
    inner :: Int -> Int -> (VU.Vector (a, Int), VU.Vector (a, Int))
    inner acc i = case rle VG.!? i of
      Nothing -> (rle, VU.empty)
      Just (!a, !dn) -> case compare (acc + dn) n of
        EQ -> VU.splitAt (i + 1) rle
        GT ->
          let l = VU.take i rle
              r = VU.drop (i + 1) rle
           in -- WARNING: We're allocating a new vector! We need real iterators. It doesn't matter
              -- for short RLE vector though.
              (VU.snoc l (a, n - acc), VU.cons (a, dn - (n - acc)) r)
        LT -> inner (acc + dn) (i + 1)

pattern NONE, ASCE, DESC :: Int
pattern NONE = 0
pattern ASCE = 1
pattern DESC = 2

freq0 :: VU.Vector Int
freq0 = VU.replicate 11 0

toFreq :: (HasCallStack) => VU.Vector Int -> VU.Vector Int
toFreq !xs = VU.accumulate (+) freq0 $! VU.map (,1) xs

-- verification-helper: PROBLEM https://atcoder.jp/contests/past202303-open/tasks/past202303_o
mainImpl :: Int -> Int -> VU.Vector Int -> VU.Vector (Int, Int, Int) -> VU.Vector Int
mainImpl n q xs0 qs = runST $ do
  let !blockLen = round (sqrt (fromIntegral n) :: Double) :: Int
  let !nBlocks = (n + blockLen - 1) `div` blockLen
  let sliceLr l r vec = VUM.take (r - l) $ VUM.drop l vec

  -- mutable sequence:
  xs <- VU.thaw xs0

  -- mutable block info:
  blockSortFlag <- VUM.replicate n NONE
  blockFreq <- VM.generate nBlocks $ \iBlock ->
    toFreq . VU.take blockLen $ VU.drop (blockLen * iBlock) xs0
  blockSum <- VUM.generate nBlocks $ \iBlock ->
    VU.sum . VU.take blockLen $ VU.drop (blockLen * iBlock) xs0

  -- Restores the sequence values in a block from `freq` at the block.
  let -- propagate :: (HasCallStack) => Int -> ST s ()
      propagate iBlock = do
        oldFlag <- VGM.exchange blockSortFlag iBlock NONE
        when (oldFlag /= NONE) $ do
          !dist <-
            if oldFlag == ASCE
              then VU.imap (,) <$> VGM.read blockFreq iBlock
              else VU.reverse . VU.imap (,) <$> VGM.read blockFreq iBlock
          let !assignments = unRle dist
          let !slice = VGM.take blockLen $ VGM.drop (blockLen * iBlock) xs
          slice' <- VU.unsafeFreeze slice
          -- let !_ = traceShow (assignments, slice') ()
          VU.iforM_ assignments $ \i !x -> do
            VGM.write slice i x

  -- -- debug helper
  -- let inspect = do
  --       for_ [0 .. nBlocks - 1] propagate
  --       xs' <- VU.unsafeFreeze xs
  --       freq' <- VG.unsafeFreeze blockFreq
  --       sums' <- VG.unsafeFreeze blockSum
  --       pure ()

  -- Reads sum.
  let -- readSumFull :: (HasCallStack) => Int -> ST s Int
      readSumFull iBlock = do
        VGM.read blockSum iBlock

  let -- readSumPart :: (HasCallStack) => Int -> Int -> Int -> ST s Int
      readSumPart iBlock l r = do
        propagate iBlock
        VU.sum <$> VU.unsafeFreeze (sliceLr l r xs)

  -- Reads freq.
  let -- readFreqFull :: (HasCallStack) => Int -> ST s (VU.Vector Int)
      readFreqFull iBlock = do
        VGM.read blockFreq iBlock

  let -- readFreqPart :: (HasCallStack) => Int -> Int -> Int -> ST s (VU.Vector Int)
      readFreqPart iBlock l r = do
        propagate iBlock
        toFreq <$> VU.unsafeFreeze (sliceLr l r xs)

  -- Assignes `dist` to the block. The internal sequence update is delayed.
  let -- assignDistFull :: (HasCallStack) => Int -> VU.Vector (Int, Int) -> Int -> ST s (VU.Vector (Int, Int))
      assignDistFull flag dist iBlock = do
        VGM.write blockSortFlag iBlock flag
        let (!use, !rest) = splitAtRle blockLen dist
        VGM.write blockSum iBlock $! VU.sum $ VU.map (\(!x, !n) -> x * n) use
        VGM.write blockFreq iBlock $! VU.accumulate (+) freq0 use
        -- let !_ = traceShow ("full", iBlock, dist) ()
        pure rest

  let -- assignDistPart :: (HasCallStack) => VU.Vector (Int, Int) -> Int -> Int -> Int -> ST s (VU.Vector (Int, Int))
      assignDistPart dist iBlock l r
        -- FIXME: is this needed?
        | l >= r = pure dist
      assignDistPart dist iBlock l r = do
        propagate iBlock

        let (!use, !rest) = splitAtRle (r - l) dist

        -- update freq.
        -- REMARK: seems like we need strict evaluation 'cause we're using `unsafeFreeze`
        -- REMARK: don't use modify
        slice <- VU.unsafeFreeze $ sliceLr l r xs
        freq <- VGM.read blockFreq iBlock
        VGM.write blockFreq iBlock $! VU.accumulate (+) freq $ use VU.++ VU.map (,-1) slice

        -- update the internal sequence and sum
        VU.iforM_ (unRle use) $ \i x -> do
          xOld <- VGM.exchange xs (l + i) x
          VGM.modify blockSum (+ (x - xOld)) iBlock

        pure rest

  res <- (`VU.imapMaybeM` qs) $ \i -> \case
    (1, pred -> !l, !r) -> do
      -- let !_ = traceShow (i, "1", l, r) ()
      -- sort [l, r) in ascending order
      -- inspect

      -- first, read freq:
      freq <- Sd.foldMapWithM blockLen (VU.zipWith (+)) readFreqFull readFreqPart l r
      let dist = VU.imap (,) freq

      -- second, assign the distribution:
      Sd.foldM_ blockLen (assignDistFull ASCE) assignDistPart dist l r

      pure Nothing
    (2, pred -> !l, !r) -> do
      -- let !_ = traceShow (i, "2", l, r) ()
      -- sort [l, r) in descending order
      -- inspect

      -- first, read freq:
      freq <- Sd.foldMapWithM blockLen (VU.zipWith (+)) readFreqFull readFreqPart l r
      let dist = VU.reverse $ VU.imap (,) freq

      -- second, assign the distribution:
      Sd.foldM_ blockLen (assignDistFull DESC) assignDistPart dist l r

      pure Nothing
    (3, pred -> !l, !r) -> do
      -- get sum in [l, r)
      -- inspect

      Just <$> Sd.foldMapWithM blockLen (+) readSumFull readSumPart l r
    _ -> error "unreachable"

  -- inspect
  pure res

main :: IO ()
main = do
  (!n, !q) <- ints2
  xs0 <- ints
  qs <- VU.replicateM q ints3
  printBSB . unlinesBSB $ mainImpl n q xs0 qs

prop_toRle :: [(Int, QC.Positive Int)] -> QC.Property
prop_toRle xns = do
  let n = sum $ map (\(!_, QC.Positive n) -> n) xns
  let m = VU.length $ unRle $ VU.fromList $ map (\(!x, QC.Positive n) -> (x, n)) xns
  n QC.=== m

prop_split :: [(Int, QC.Positive Int)] -> QC.Gen QC.Property
prop_split xns = do
  let xns' = VU.fromList $ map (\(!x, QC.Positive n) -> (x, n)) xns
  at <- QC.chooseInt (0, VU.length xns')
  let len = VU.sum $ VU.map snd xns'
  let (!l, !r) = splitAtRle at xns'
  pure $ len QC.=== (VU.length (unRle l) + VU.length (unRle r))

runP :: IO ()
runP = QC.quickCheck (QC.withMaxSuccess 1000 prop_toRle)

runQ :: IO ()
runQ = QC.quickCheck (QC.withMaxSuccess 1000 prop_split)

propQC :: QC.Gen QC.Property
propQC = do
  n <- QC.choose (1, maxN)
  q <- QC.choose (1, maxN)
  xs <- VU.fromList <$> QC.vectorOf n (QC.choose (1, 10))
  qs <- (VU.fromList <$>) $ QC.vectorOf q $ do
    k <- QC.chooseInt (1, 3)
    l <- QC.chooseInt (1, n)
    r <- QC.chooseInt (l, n)
    pure (k, l, r)
  let !_ = unsafePerformIO $ do
        result <- try (evaluate (mainImpl n q xs qs)) :: IO (Either SomeException (VU.Vector Int))
        case result of
          Left ex -> do
            putStrLn $ "Caught an exception: " ++ show ex
            print ("input", n, q, xs, qs)
            exitSuccess
          Right _ -> pure ()
  pure $ True QC.=== True
  where
    maxN = 7 -- 1000
    maxQ = 7 -- 1000

runQC :: IO ()
runQC = QC.quickCheck (QC.withMaxSuccess 1000 propQC)
