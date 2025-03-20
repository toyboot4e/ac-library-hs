{-# LANGUAGE RecordWildCards #-}

-- | [Square root decomposition](https://cp-algorithms.com/data_structures/sqrt_decomposition.html) for a sequence.
--
-- ==== Performance
-- Square root decomposition often results in TLE.
--
-- @since 1.2.3.0
module AtCoder.Extra.SqrtDecomposition
  ( -- Block (..),
  -- SqrtDecomposition (..)
  )
where

import Control.Monad (unless, when)
import Control.Monad.Primitive (PrimMonad, PrimState, stToPrim)
import Control.Monad.ST
import Data.Bits
import Data.Foldable (for_)
import Data.Vector qualified as V
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM

-- -- | View to a block in `SqrtDecomposition` sequence.
-- data Block s p f a = Block
--   { -- | Left index
--     lB :: {-# UNPACK #-} !Int,
--     -- | Right index
--     rB :: {-# UNPACK #-} !Int,
--     -- | Monoid product.
--     pB :: p,
--     -- | Monoid action.
--     fB :: f,
--     -- | Acted monoids.
--     xB :: VUM.MVector s a
--   }

-- class BlockRead cx p f a where
--   mergeSD :: cx -> a -> a -> a
--
--   readFullSD :: cx -> p -> a
--
--   -- - propagate the monoid action if necessary
--   -- - read the leaf values
--   readPartSD ::
--     -- | Context for the read query
--     cx ->
--     -- | View to a block
--     Block s p f a ->
--     -- | Local index \(l\)
--     Int ->
--     -- | Local index \(r\)
--     Int ->
--     -- | Product of \([l, r)\)
--     ST s a

-- actFullSD :: Block s p f a -> ST s ()

-- actPartSD :: Int -> f -> Int -> Int -> m ()

-- -- | Represents a sequence of values.
-- data SqrdDecomposition s p f a = SqrdDecomposition
--   { blockLenSD :: {-# UNPACK #-} !Int,
--     blocksSD :: !(V.Vector (Block s p f a))
--   }
--
-- -- TODO: propagate before folding
--
-- -- | \(O(\sqrt N f)\)
-- fold :: (PrimMonad m) => SqrtDecomposition b ret act m -> Int -> Int -> m ret
-- fold SqrtDecomposition {..} l r = do
--   let (!il, !remL) = l `divMod` blockLenSD
--   let (!ir, !remR) = r `divMod` blockLenSD
--   if il == ir
--     then do
--       readPartSD il remL remR
--     else do
--       !lx <- readPartSD il remL (blockLenSD - 1)
--       !mx <- U.foldM' (\ !acc -> mergeSD acc <=< readFullSD) lx $ U.generate (ir - 1 - il) (+ (il + 1))
--       !rx <- readPartSD ir 0 remR
--       !ret <- mergeSD mx rx
--       pure ret
--
-- -- | \(O(\sqrt N f)\)
-- actSD :: (PrimMonad m) => SqrtDecomposition b ret act m -> act -> Int -> Int -> m ()
-- actSD SD {..} act l r = do
--   let (!il, !remL) = l `divMod` blockLenSD
--   let (!ir, !remR) = r `divMod` blockLenSD
--   if il == ir
--     then do
--       actPartSD il act remL remR
--     else do
--       actPartSD il act remL (blockLenSD - 1)
--       U.mapM_ (`actFullSD` act) $ U.generate (ir - 1 - il) (+ (il + 1))
--       actPartSD ir act 0 remR
