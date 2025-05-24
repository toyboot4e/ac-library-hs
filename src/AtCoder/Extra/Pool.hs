{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- | Fixed-sized array for \(O(1)\) allocation and \(O(1)\) clearing after \(O(n)\) construction.
module AtCoder.Extra.Pool
  ( -- * Pool
    Pool (..),
    Index (..),
    undefIndex,
    nullIndex,

    -- * Constructors
    new,
    clear,

    -- * Metadata
    capacity,
    size,

    -- * Allocations
    alloc,
    free,

    -- * Read/write
    read,
    write,
    modify,
    exchange,

    -- * Handle
    Handle (..),
    newHandle,
    nullHandle,
    invalidateHandle,
  )
where

import AtCoder.Internal.Buffer qualified as B
import Control.Monad.Primitive (PrimMonad, PrimState, stToPrim)
import Control.Monad.ST (ST)
import Data.Coerce
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Primitive qualified as VP
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Stack (HasCallStack)
import Prelude hiding (read)

-- | Fixed-size array for \(O(1)\) allocation and \(O(1)\) clearing after \(O(n)\) construction.
--
-- @since 1.2.0.0
data Pool s a = Pool
  { -- | Data array.
    dataPool :: !(VUM.MVector s a),
    -- | Free slot indices pushed on free.
    freePool :: !(B.Buffer s Index),
    -- | Next index when `freePool` is empty.
    nextPool :: !(VUM.MVector s Index)
  }

-- | Strongly typed index of pool items. User has to explicitly @corece@ on raw index use.
--
-- @since 1.2.0.0
newtype Index = Index {unIndex :: Int}
  deriving
    ( -- | @since 1.2.0.0
      Eq,
      -- | @since 1.2.0.0
      VP.Prim
    )
  deriving newtype (Ord, Show)

-- | @since 1.2.0.0
newtype instance VU.MVector s Index = MV_Index (VP.MVector s Index)

-- | @since 1.2.0.0
newtype instance VU.Vector Index = V_Index (VP.Vector Index)

-- | @since 1.2.0.0
deriving via (VU.UnboxViaPrim Index) instance VGM.MVector VUM.MVector Index

-- | @since 1.2.0.0
deriving via (VU.UnboxViaPrim Index) instance VG.Vector VU.Vector Index

-- | @since 1.2.0.0
instance VU.Unbox Index

-- | Invalid, null `Index`.
--
-- @since 1.2.0.0
{-# INLINE undefIndex #-}
undefIndex :: Index
undefIndex = Index (-1)

-- | Returns `True` for `undefIndex`.
--
-- @since 1.2.0.0
{-# INLINE nullIndex #-}
nullIndex :: Index -> Bool
nullIndex = (== undefIndex)

-- | \(O(n)\) Creates a pool with the specified @capacity@.
--
-- @since 1.2.0.0
{-# INLINE new #-}
new :: (VU.Unbox a, PrimMonad m) => Int -> m (Pool (PrimState m) a)
new cap = stToPrim $ newST cap

-- | \(O(1)\) Resets the pool to the initial state.
--
-- @since 1.2.0.0
{-# INLINE clear #-}
clear :: (PrimMonad m) => Pool (PrimState m) a -> m ()
clear pool = stToPrim $ clearST pool

-- | \(O(1)\) Returns the maximum number of elements the pool can store.
--
-- @since 1.2.0.0
{-# INLINE capacity #-}
capacity :: (VU.Unbox a) => Pool s a -> Int
capacity = VGM.length . dataPool

-- | \(O(1)\) Returns the number of elements in the pool.
--
-- @since 1.2.0.0
{-# INLINE size #-}
size :: (PrimMonad m, VU.Unbox a) => Pool (PrimState m) a -> m Int
size pool = stToPrim $ sizeST pool

-- | \(O(1)\) Allocates a new element.
--
-- ==== Constraints
-- - The number of elements must not exceed the `capacity`.
--
-- @since 1.2.0.0
{-# INLINE alloc #-}
alloc :: (HasCallStack, PrimMonad m, VU.Unbox a) => Pool (PrimState m) a -> a -> m Index
alloc pool x = stToPrim $ allocST pool x

-- | \(O(1)\) Frees an element. Be sure to not free a deleted element.
--
-- ==== Constraints
-- - \(0 \le i \lt n\)
--
-- @since 1.2.0.0
{-# INLINE free #-}
free :: (PrimMonad m) => Pool (PrimState m) a -> Index -> m ()
free Pool {..} i = do
  B.pushBack freePool i

-- | \(O(1)\) Reads the \(k\)-th value.
--
-- ==== Constraints
-- - \(0 \le i \lt n\)
--
-- @since 1.2.0.0
{-# INLINE read #-}
read :: (PrimMonad m, VU.Unbox a) => Pool (PrimState m) a -> Index -> m a
read Pool {dataPool} !i = do
  VGM.read dataPool (coerce i)

-- | \(O(1)\) Writes to the \(k\)-th value.
--
-- ==== Constraints
-- - \(0 \le i \lt n\)
--
-- @since 1.2.0.0
{-# INLINE write #-}
write :: (PrimMonad m, VU.Unbox a) => Pool (PrimState m) a -> Index -> a -> m ()
write Pool {dataPool} !i !x = do
  VGM.write dataPool (coerce i) x

-- | \(O(1)\) Given a user function \(f\), modifies the \(k\)-th value with it.
--
-- ==== Constraints
-- - \(0 \le i \lt n\)
--
-- @since 1.2.0.0
{-# INLINE modify #-}
modify :: (PrimMonad m, VU.Unbox a) => Pool (PrimState m) a -> (a -> a) -> Index -> m ()
modify Pool {dataPool} !f !i = do
  VGM.modify dataPool f (coerce i)

-- | \(O(1)\) Exchanges the \(k\)-th value.
--
-- ==== Constraints
-- - \(0 \le i \lt n\)
--
-- @since 1.2.0.0
{-# INLINE exchange #-}
exchange :: (PrimMonad m, VU.Unbox a) => Pool (PrimState m) a -> Index -> a -> m a
exchange Pool {dataPool} !i !x = do
  VGM.exchange dataPool (coerce i) x

-- | Mutable `Handle` of an `Index`.
--
-- @since 1.2.0.0
newtype Handle s = Handle
  { -- | @since 1.2.0.0
    unHandle :: VUM.MVector s Index
  }

-- | \(O(1)\) Creates a new `Handle` from a root node index.
--
-- @since 1.2.0.0
{-# INLINE newHandle #-}
newHandle :: (PrimMonad m) => Index -> m (Handle (PrimState m))
newHandle x = Handle <$> VUM.replicate 1 x

-- | \(O(1)\) Returns whether the handle represents null.
--
-- @since 1.2.0.0
{-# INLINE nullHandle #-}
nullHandle :: (PrimMonad m) => Handle (PrimState m) -> m Bool
nullHandle (Handle h) = nullIndex <$> VGM.unsafeRead h 0

-- | \(O(1)\) Invalidates a handle. Note that it does not change or `free` the pool item.
--
-- @since 1.2.0.0
{-# INLINE invalidateHandle #-}
invalidateHandle :: (PrimMonad m) => Handle (PrimState m) -> m ()
invalidateHandle (Handle h) = VGM.unsafeWrite h 0 undefIndex

-- -------------------------------------------------------------------------------------------------
-- Internal
-- -------------------------------------------------------------------------------------------------

{-# INLINEABLE newST #-}
newST :: (VU.Unbox a) => Int -> ST s (Pool s a)
newST cap = do
  dataPool <- VUM.unsafeNew cap
  freePool <- B.new cap
  nextPool <- VUM.replicate 1 (Index 0)
  pure Pool {..}

{-# INLINEABLE clearST #-}
clearST :: Pool s a -> ST s ()
clearST Pool {..} = do
  B.clear freePool
  VGM.unsafeWrite nextPool 0 $ Index 0

{-# INLINEABLE sizeST #-}
sizeST :: (VU.Unbox a) => Pool s a -> ST s Int
sizeST Pool {..} = do
  !nFree <- B.length freePool
  Index !next <- VGM.unsafeRead nextPool 0
  pure $ next - nFree

{-# INLINEABLE allocST #-}
allocST :: (HasCallStack, VU.Unbox a) => Pool s a -> a -> ST s Index
allocST Pool {..} !x = do
  B.popBack freePool >>= \case
    Just i -> pure i
    Nothing -> do
      Index i <- VGM.unsafeRead nextPool 0
      if i < VGM.length dataPool
        then do
          VGM.unsafeWrite nextPool 0 $ coerce (i + 1)
          VGM.write dataPool i x
          pure $ coerce i
        else do
          error "AtCoder.Extra.Pool.allocST: capacity out of bounds"
