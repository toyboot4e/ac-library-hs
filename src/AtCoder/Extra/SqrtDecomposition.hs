-- | Square root decomposition is a technique that divides a sequence of values into around
-- \(\sqrt n\) blocks, aggregating the state information for each block. It allows user to process
-- interval query block by block, typically in \(O(\sqrt n)\) time, where a whole block processing
-- take \(O(1)\) time and partial block processing take \(O(\sqrt n)\) time.
--
-- For simplicity, in this document, we assume that highder order functions applided to an entier
-- block (@readFull@ and @actFull@) work in \(O(1)\) time, and those applied to a part of block work
-- in \(O(\sqrt n)\) time. In total, \(q\) query processing takes \(O(q \sqrt n)\) time. Note that
-- it's a rather large number and often requires performance tuning.
--
-- ==== Lazy propagation
-- Typiaclly, an action to a whole block can be delayed; store the aggregation value for the block,
-- delay the internal sequence update, and restore them when part of the block is accessed. Such
-- lazy propagation should be handled on the user side on partial block access functions
-- (@foldPart@ or @actPart@) are called.
--
-- @since 1.2.5.0
module AtCoder.Extra.SqrtDecomposition
  ( -- | These function signatures try to resemble those for lists.
    forM_,
    foldMapM,
    foldMapWithM,
    foldM,
    foldM_,
  )
where

import AtCoder.Internal.Assert qualified as ACIA
import Control.Monad (when)
import Data.Foldable (for_)
import Data.Vector.Unboxed qualified as VU

-- INLINE all the functions, even if the performance gain is just a little bit, in case it matters.

-- | \(O(\sqrt n)\) Runs user function for each block.
{-# INLINE forM_ #-}
forM_ ::
  (Monad m) =>
  -- | Context: block length.
  Int ->
  -- | Function: @actFull@ function that takes target block index.
  (Int -> m ()) ->
  -- | Function: @actPart@ function that takes target block index, left index and right index.
  (Int -> Int -> Int -> m ()) ->
  -- | Input: \(l\).
  Int ->
  -- | Input: \(r\).
  Int ->
  -- | Unit.
  m ()
forM_ !blockLen !actFull !actPart !l !r = do
  let !_ = ACIA.runtimeAssert (l <= r) "AtCoder.Extra.SqrtDecomposition.forM_: `l <= r` must hold"
  let (!il, !remL) = l `divMod` blockLen
  let (!ir, !remR) = r `divMod` blockLen
  if il == ir
    then do
      when (remR > remL) $ do
        actPart il l r
    else do
      if remL == 0
        then actFull il
        else actPart il l (l - remL + blockLen)
      for_ [il + 1 .. ir - 1] $ \iBlock -> do
        actFull iBlock
      when (remR > 0) $ do
        actPart ir (r - remR) r

-- | \(O(\sqrt n)\) Runs user function for each block and concatanates their monoid output.
--
-- ==== Constraints
-- - \(l \le r\)
-- - If an empty interval is queried, the @readPart@ function must return a valid value.
--
-- @since 1.2.5.0
{-# INLINE foldMapM #-}
foldMapM ::
  (Monad m, Semigroup a) =>
  -- | Context: block length.
  Int ->
  -- | Function: @readFull@ function that takes target block index and returns monoid value of it.
  (Int -> m a) ->
  -- | Function: @readPart@ function that takes target block index, left index and right index, and
  -- returns monoid value for it.
  (Int -> Int -> Int -> m a) ->
  -- | Input: \(l\).
  Int ->
  -- | Input: \(r\).
  Int ->
  -- | Concatenated output.
  m a
foldMapM blockLen = foldMapWithM blockLen (<>)

-- | \(O(\sqrt n)\) Runs user function for each block and concatanates their output with user
-- function.
--
-- ==== Constraints
-- - \(l \le r\)
-- - If an empty interval is queried, the @readPart@ function must return a valid value.
--
-- @since 1.2.5.0
{-# INLINE foldMapWithM #-}
foldMapWithM ::
  (Monad m) =>
  -- | Context: block length.
  Int ->
  -- | Merges function for output values.
  (a -> a -> a) ->
  -- | Function: @readFull@ function that takes target block index and returns monoid value of it.
  (Int -> m a) ->
  -- | Function: @readPart@ function that takes target block index, left index and right index, and
  -- returns output value of it.
  (Int -> Int -> Int -> m a) ->
  -- | Input: \(l\).
  Int ->
  -- | Input: \(r\).
  Int ->
  -- | Concatenated output.
  m a
foldMapWithM !blockLen !merge !readFull !readPart !l !r = do
  let !_ = ACIA.runtimeAssert (l <= r) "AtCoder.Extra.SqrtDecomposition.foldMapWithM: `l <= r` must hold"
  let (!il, !remL) = l `divMod` blockLen
  let (!ir, !remR) = r `divMod` blockLen
  if il == ir
    then do
      readPart il l r
    else do
      !sx <-
        if remL == 0
          then readFull il
          else readPart il l (l - remL + blockLen)
      !sm <-
        VU.foldM'
          (\ !acc iBlock -> merge acc <$> readFull iBlock)
          sx
          $ VU.generate (ir - 1 - il) (+ (il + 1))
      if remR == 0
        then pure sm
        else do
          rx <- readPart ir (r - remR) r
          pure $! merge sm rx

-- | \(O(\sqrt n)\) Runs user function for each block, performing left folding.
--
-- ==== Constraints
-- - \(l \le r\)
--
-- @since 1.2.5.0
{-# INLINE foldM #-}
foldM ::
  (Monad m) =>
  -- | Context: block length.
  Int ->
  -- | Function: @foldFull@ function that takes target block index and returns monoid value of it.
  (a -> Int -> m a) ->
  -- | Function: @foldPart@ function that takes target block index, left and right local index and returns monoid
  -- value of it.
  (a -> Int -> Int -> Int -> m a) ->
  -- | Initial folding value.
  a ->
  -- | Input: \(l\).
  Int ->
  -- | Input: \(r\).
  Int ->
  -- | Folding result.
  m a
foldM !blockLen !foldFull !foldPart !s0 !l !r = do
  let !_ = ACIA.runtimeAssert (l <= r) "AtCoder.Extra.SqrtDecomposition.foldM: `l <= r` must hold"
  let (!il, !remL) = l `divMod` blockLen
  let (!ir, !remR) = r `divMod` blockLen
  if il == ir
    then do
      if remL == remR
        then pure s0
        else foldPart s0 il l r
    else do
      !sx <-
        if remL == 0
          then foldFull s0 il
          else foldPart s0 il l (l - remL + blockLen)
      !sm <-
        VU.foldM'
          foldFull
          sx
          $ VU.generate (ir - 1 - il) (+ (il + 1))
      if remR == 0
        then pure sm
        else foldPart sm ir (r - remR) r

-- | \(O(\sqrt n)\) `foldM` with return value discarded.
--
-- ==== Constraints
-- - \(l \le r\)
--
-- @since 1.2.5.0
{-# INLINE foldM_ #-}
foldM_ ::
  (Monad m) =>
  -- | Context: Block length.
  Int ->
  -- | @readFull@ function that takes target block index and returns monoid value of it.
  (a -> Int -> m a) ->
  -- | @readPart@ function that takes target block index, left and right local index and returns monoid
  -- value of it.
  (a -> Int -> Int -> Int -> m a) ->
  -- | Initial folding value.
  a ->
  -- | Input: \(l\).
  Int ->
  -- | Input: \(r\).
  Int ->
  -- | Unit.
  m ()
foldM_ !blockLen !readFull !readPart !s0 !l !r = do
  _ <- foldM blockLen readFull readPart s0 l r
  pure ()
