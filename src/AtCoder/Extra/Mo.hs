-- | Mo's algorithm for handling \([l, r)\) offline queries in \(O((n + q) \sqrt n f)\) time
-- complecity, where \(n\) is the length of index, \(q\) is the number of queries and \(f\) is the
-- time for processing element addition or deletion. Due to the high time complexity, it is
-- recommended to choose an efficient data structure such as Fenwick Tree for query processing.
--
-- @since 1.2.5.0
module AtCoder.Extra.Mo
  ( run,
    sortIndices,
    process,
  )
where

import Control.Monad (when)
import Control.Monad.Primitive (PrimMonad)
import Data.Bool (bool)
import Data.Foldable (for_)
import Data.Vector.Algorithms.Intro qualified as VAI
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Stack (HasCallStack)

-- | \(O((n + q) \sqrt n)\) Runs Mo's algorithm. Internally it's a call of `sortIndices` and
-- `process`.
--
-- @since 1.2.5.0
{-# INLINE run #-}
run ::
  (HasCallStack, PrimMonad m, VU.Unbox a) =>
  -- | Defines index bounds \([0, n)\).
  Int ->
  -- | Query intervals \([l, r)\).
  VU.Vector (Int, Int) ->
  -- | Called on adding left index \(l\).
  (Int -> m ()) ->
  -- | Called on adding left index \(r\).
  (Int -> m ()) ->
  -- | Called on deleting left index \(l\).
  (Int -> m ()) ->
  -- | Called on deleting right index \(r\).
  (Int -> m ()) ->
  -- | Returns result for query index \(i\).
  (Int -> m a) ->
  -- | Result for each query.
  m (VU.Vector a)
run n !lrs !addL !addR !delL !delR !query = do
  let !is = sortIndices n lrs
  process is lrs addL addR delL delR query

-- | \(O(n (\log n))\) Sorts indices of \([l, r)\) queries in an efficient order for processing.
--
-- @since 1.2.5.0
{-# INLINEABLE sortIndices #-}
sortIndices ::
  (HasCallStack) =>
  -- | Defines index bounds \([0, n)\).
  Int ->
  -- | Query intervals \([l, r)\).
  VU.Vector (Int, Int) ->
  -- | Sorted indices to the query intervals.
  VU.Vector Int
sortIndices n !lrs
  | VU.null lrs = VU.empty
  | otherwise = VU.create $ do
      let !q = VU.length lrs
      let !blockLen :: Int = max 1 $ round (sqrt 3 * fromIntegral n / sqrt (fromIntegral (2 * q)) :: Double)
      is <- VUM.generate q id

      -- sort by block index then right:
      VAI.sortBy
        ( \i1 i2 -> do
            let (!l1, !r1) = lrs VG.! i1
                (!l2, !r2) = lrs VG.! i2
                !b1 = l1 `div` blockLen
                !b2 = l2 `div` blockLen
                !res = compare b1 b2 <> bool (compare r2 r1) (compare r1 r2) (even b1)
             in res
        )
        is

      -- The following trick doesn't seem to make it faster though?

      let -- {-# INLINE cost #-}
          cost i1 i2 = do
            (!l1, !r1) <- (lrs VG.!) <$> VGM.read is i1
            (!l2, !r2) <- (lrs VG.!) <$> VGM.read is i2
            pure $ abs (l1 - l2) + abs (r1 - r2)

      when (q > 6) $ do
        for_ [0 .. q - 6] $ \k -> do
          do
            c1 <- cost k (k + 2)
            c2 <- cost (k + 1) (k + 3)
            c3 <- cost k (k + 1)
            c4 <- cost (k + 2) (k + 3)
            when (c1 + c2 < c3 + c4) $ do
              VGM.swap is (k + 1) (k + 2)
          do
            c1 <- cost k (k + 3)
            c2 <- cost (k + 1) (k + 4)
            c3 <- cost k (k + 1)
            c4 <- cost (k + 3) (k + 4)
            when (c1 + c2 < c3 + c4) $ do
              VGM.swap is (k + 1) (k + 3)

      pure is

-- | \(O((n + q) \sqrt n)\) Processes \([l, r)\) interval queries. User would usually use `run`
-- instead.
--
-- @since 1.2.5.0
{-# INLINEABLE process #-}
process ::
  (HasCallStack, PrimMonad m, VU.Unbox a) =>
  -- | Sorted indices to query intervals \([l, r)\).
  VU.Vector Int ->
  -- | Query intervals \([l, r)\).
  VU.Vector (Int, Int) ->
  -- | Called on adding left index \(l\).
  (Int -> m ()) ->
  -- | Called on adding right index \(r\).
  (Int -> m ()) ->
  -- | Called on deleting left index \(l\).
  (Int -> m ()) ->
  -- | Called on deleting right index \(r\).
  (Int -> m ()) ->
  -- | Returns result for query index \(i\).
  (Int -> m a) ->
  -- | Result for each query.
  m (VU.Vector a)
process !is !lrs !addL !addR !delL !delR !query = do
  let !q = VU.length lrs
  !result <- VUM.unsafeNew q

  VU.foldM'_
    ( \(!l0, !r0) i -> do
        let (!l, !r) = lrs VG.! i
        for_ [l0 - 1, l0 - 2 .. l] addL
        for_ [r0, r0 + 1 .. r - 1] addR
        for_ [l0, l0 + 1 .. l - 1] delL
        for_ [r0 - 1, r0 - 2 .. r] delR
        VGM.unsafeWrite result i =<< query i
        pure (l, r)
    )
    (0 :: Int, 0 :: Int)
    is

  VU.unsafeFreeze result
