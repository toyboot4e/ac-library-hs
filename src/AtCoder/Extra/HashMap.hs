{-# LANGUAGE RecordWildCards #-}

-- original implementaion:
-- <https://github.com/maspypy/library/blob/main/ds/hashmap.hpp>

-- | A dense, fast `Int` hash map with a fixed-sized `capacity` of \(n\). Most operations are
-- performed in \(O(1)\) time, but in average.
--
-- ==== Capacity limitation
-- Access to each key creates a new entry. Note that entries cannot be invalidated due to the
-- internal implementation (called /open addressing/).
--
-- ==== __Example__
-- Create a `HashMap` with `capacity` \(10\):
--
-- >>> import AtCoder.Extra.HashMap qualified as HM
-- >>> hm <- HM.new @_ @Int 10
--
-- `insert`, `lookup` and other functions are available in \(O(1)\) averaged time:
--
-- >>> HM.insert hm 0 100
-- >>> HM.insert hm 10 101
-- >>> HM.size hm
-- 2
--
-- >>> HM.lookup hm 0
-- Just 100
--
-- >>> HM.lookup hm 10
-- Just 101
--
-- @since 1.1.0.0
module AtCoder.Extra.HashMap
  ( -- * HashMap
    HashMap,

    -- * Constructors
    new,
    build,

    -- * Metadata
    capacity,
    size,

    -- * Lookups
    lookup,
    member,
    notMember,

    -- * Modifications

    -- ** Insertions
    insert,
    insertWith,
    exchange,

    -- ** Updates
    modify,
    modifyM,

    -- ** Reset
    clear,

    -- * Conversions

    -- ** Safe conversions
    keys,
    elems,
    assocs,

    -- ** Unsafe conversions
    unsafeKeys,
    unsafeElems,
    unsafeAssocs,
  )
where

import AtCoder.Internal.Assert qualified as ACIA
import Control.Monad (void, when)
import Control.Monad.ST (ST)
import Control.Monad.Primitive (PrimMonad, PrimState, stToPrim)
import Data.Bit (Bit (..))
import Data.Bits (Bits (xor, (.&.)), (.>>.))
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import Data.Word (Word64)
import GHC.Stack (HasCallStack)
import Prelude hiding (lookup)

-- | A dense, fast `Int` hash map with a fixed-sized capacity of \(n\).
--
-- @since 1.1.0.0
data HashMap s a = HashMap
  { -- | Maximum number of elements.
    maxCapHM :: {-# UNPACK #-} !Int,
    -- | The number of elements that can be added.
    restCapHM :: !(VUM.MVector s Int),
    -- | Bit mask for powerset iteration on indexing.
    maskHM :: {-# UNPACK #-} !Int,
    -- | Original key to the hash index.
    keyHM :: !(VUM.MVector s Int),
    -- | Values to the hash index.
    valHM :: !(VUM.MVector s a),
    -- | Whether the slot is used or not.
    usedHM :: !(VUM.MVector s Bit)
  }

{-# INLINE decrementRestCapacity #-}
decrementRestCapacity :: (HasCallStack, PrimMonad m) => VUM.MVector (PrimState m) Int -> String -> m ()
decrementRestCapacity restCap funcName = do
  rest <- VGM.unsafeRead restCap 0
  let !_ = ACIA.runtimeAssert (rest > 0) $ "AtCoder.Extra.HashMap." ++ funcName ++ ": out of capacity"
  VGM.unsafeWrite restCap 0 (rest - 1)
  pure ()

-- | \(O(n)\) Creates a `HashMap` of capacity \(n\).
--
-- @since 1.1.0.0
{-# INLINE new #-}
new :: (PrimMonad m, VU.Unbox a) => Int -> m (HashMap (PrimState m) a)
new n = do
  let !k0 = 1
  let !k = until (>= 2 * n) (* 2) k0
  -- we need extra space
  let !maxCapHM = k `div` 2
  restCapHM <- VUM.replicate 1 maxCapHM
  let !maskHM = k - 1
  keyHM <- VUM.unsafeNew k
  valHM <- VUM.unsafeNew k
  usedHM <- VUM.replicate k $ Bit False
  pure HashMap {..}

-- | \(O(n)\) Creates a `HashMap` of capacity \(n\) with initial entries.
--
-- @since 1.1.0.0
{-# INLINE build #-}
build :: (PrimMonad m, VU.Unbox a) => Int -> VU.Vector (Int, a) -> m (HashMap (PrimState m) a)
build n xs = do
  hm <- new n
  VU.forM_ xs $ \(!i, !x) -> do
    insert hm i x
  pure hm

-- | \(O(1)\) Returns the maximum number of elements the hash map can store.
--
-- @since 1.1.0.0
{-# INLINE capacity #-}
capacity :: HashMap s a -> Int
capacity = maxCapHM

-- | \(O(1)\) Returns the number of elements in the hash map.
--
-- @since 1.1.0.0
{-# INLINE size #-}
size :: (PrimMonad m) => HashMap (PrimState m) a -> m Int
size HashMap {..} = do
  !rest <- VUM.unsafeRead restCapHM 0
  pure $ maxCapHM - rest

-- | \(O(1)\) (Internal) Hash value calculation.
--
-- @since 1.1.0.0
{-# INLINE hash #-}
hash :: HashMap a s -> Int -> Int
hash hm x = fromIntegral $ (x3 `xor` (x3 .>>. 31)) .&. fromIntegral (maskHM hm)
  where
    fixedRandom, x1, x2, x3 :: Word64
    fixedRandom = 321896566547
    x1 = fromIntegral x + fixedRandom
    x2 = (x1 `xor` (x1 .>>. 30)) * 0xbf58476d1ce4e5b9
    x3 = (x2 `xor` (x2 .>>. 27)) * 0x94d049bb133111eb

-- | \(O(1)\) (Internal) Hashed slot search.
--
-- ==== Constraint
-- - The rest capacity must be non-zero. Otherwise it loops forever.
{-# INLINE indexST #-}
indexST :: (HasCallStack) => HashMap s a -> Int -> ST s Int
indexST hm@HashMap {..} k = do
  inner (hash hm k)
  where
    inner !h = do
      Bit b <- VGM.read usedHM h
      -- already there?
      k' <- VGM.read keyHM h
      if b && k' /= k
        then inner $ (h + 1) .&. maskHM
        else pure h

-- | \(O(1)\) Return the value to which the specified key is mapped, or `Nothing` if this map
-- contains no mapping for the key.
--
-- ==== Constraint
-- - The rest capacity must be non-zero. Otherwise it loops forever.
--
-- @since 1.1.0.0
{-# INLINE lookup #-}
lookup :: (HasCallStack, VU.Unbox a, PrimMonad m) => HashMap (PrimState m) a -> Int -> m (Maybe a)
lookup hm@HashMap {..} k = do
  i <- stToPrim $ indexST hm k
  Bit b <- VGM.read usedHM i
  if b
    then Just <$> VGM.read valHM i
    else pure Nothing

-- | \(O(1)\) Checks whether the hash map contains the element.
--
-- ==== Constraint
-- - The rest capacity must be non-zero. Otherwise it loops forever.
--
-- @since 1.1.0.0
{-# INLINE member #-}
member :: (HasCallStack, PrimMonad m) => HashMap (PrimState m) a -> Int -> m Bool
member hm@HashMap {..} k = do
  i <- stToPrim $ indexST hm k
  Bit b <- VGM.read usedHM i
  -- TODO: is this key check necessary
  k' <- VGM.read keyHM i
  pure $ b && k' == k

-- | \(O(1)\) Checks whether the hash map does not contain the element.
--
-- ==== Constraint
-- - The rest capacity must be non-zero. Otherwise it loops forever.
--
-- @since 1.1.0.0
{-# INLINE notMember #-}
notMember :: (HasCallStack, PrimMonad m) => HashMap (PrimState m) a -> Int -> m Bool
notMember hm k = not <$> member hm k

-- | \(O(1)\) Inserts a \((k, v)\) pair.
--
-- ==== Constraint
-- - The rest capacity must be non-zero. Otherwise it loops forever.
--
-- @since 1.1.0.0
{-# INLINE insert #-}
insert :: (HasCallStack, PrimMonad m, VU.Unbox a) => HashMap (PrimState m) a -> Int -> a -> m ()
insert hm k v = void $ exchange hm k v

-- | \(O(1)\) Inserts a \((k, v)\) pair. If the key exists, the function will insert the pair
-- \((k, f(v_{\mathrm{new}}, v_{\mathrm{old}}))\).
--
-- ==== Constraint
-- - The rest capacity must be non-zero. Otherwise it loops forever.
--
-- @since 1.1.0.0
{-# INLINE insertWith #-}
insertWith :: (HasCallStack, PrimMonad m, VU.Unbox a) => HashMap (PrimState m) a -> (a -> a -> a) -> Int -> a -> m ()
insertWith hm@HashMap {..} f k v = do
  i <- stToPrim $ indexST hm k
  Bit b <- VGM.exchange usedHM i $ Bit True
  if b
    then do
      -- modify the existing entry
      VGM.modify valHM (f v) i
    else do
      -- insert the new \((k, v)\) pair
      decrementRestCapacity restCapHM "insertWith"
      VGM.write keyHM i k
      VGM.write valHM i v

-- | \(O(1)\) Inserts a \((k, v)\) pair and returns the old value, or `Nothing` if no such entry
-- exists.
--
-- ==== Constraint
-- - The rest capacity must be non-zero. Otherwise it loops forever.
--
-- @since 1.1.0.0
{-# INLINE exchange #-}
exchange :: (HasCallStack, PrimMonad m, VU.Unbox a) => HashMap (PrimState m) a -> Int -> a -> m (Maybe a)
exchange hm@HashMap {..} k v = do
  i <- stToPrim $ indexST hm k
  Bit b <- VGM.exchange usedHM i $ Bit True
  if b
    then do
      -- overwrite the existing entry
      Just <$> VGM.exchange valHM i v
    else do
      -- insert the new (key, value) pair
      decrementRestCapacity restCapHM "exchange"
      VGM.write keyHM i k
      VGM.write valHM i v
      pure Nothing

-- | \(O(1)\) Modifies the element at the given key. Does nothing if no such entry exists.
--
-- @since 1.1.0.0
{-# INLINE modify #-}
modify :: (HasCallStack, PrimMonad m, VU.Unbox a) => HashMap (PrimState m) a -> (a -> a) -> Int -> m ()
modify hm@HashMap {..} f k = do
  i <- stToPrim $ indexST hm k
  Bit b <- VGM.read usedHM i
  when b $ do
    VGM.modify valHM f i

-- | \(O(1)\) Modifies the element at the given key. Does nothing if no such entry exists.
--
-- @since 1.1.0.0
{-# INLINE modifyM #-}
modifyM :: (HasCallStack, PrimMonad m, VU.Unbox a) => HashMap (PrimState m) a -> (a -> m a) -> Int -> m ()
modifyM hm@HashMap {..} f k = do
  i <- stToPrim $ indexST hm k
  Bit b <- VGM.read usedHM i
  when b $ do
    VGM.modifyM valHM f i

-- | \(O(n)\) Clears all the elements.
--
-- @since 1.1.0.0
{-# INLINE clear #-}
clear :: (PrimMonad m) => HashMap (PrimState m) a -> m ()
clear HashMap {..} = do
  VGM.set usedHM $ Bit False
  VUM.unsafeWrite restCapHM 0 maxCapHM

-- | \(O(n)\) Enumerates the keys in the hash map.
--
-- @since 1.1.0.0
{-# INLINE keys #-}
keys :: (PrimMonad m, VU.Unbox a) => HashMap (PrimState m) a -> m (VU.Vector Int)
keys hm = VU.force <$> unsafeKeys hm

-- | \(O(n)\) Enumerates the elements (values) in the hash map.
--
-- @since 1.1.0.0
{-# INLINE elems #-}
elems :: (PrimMonad m, VU.Unbox a) => HashMap (PrimState m) a -> m (VU.Vector a)
elems hm = VU.force <$> unsafeElems hm

-- | \(O(n)\) Enumerates the key-value pairs in the hash map.
--
-- @since 1.1.0.0
{-# INLINE assocs #-}
assocs :: (PrimMonad m, VU.Unbox a) => HashMap (PrimState m) a -> m (VU.Vector (Int, a))
assocs hm = VU.force <$> unsafeAssocs hm

-- | \(O(n)\) Enumerates the keys in the hash map.
--
-- @since 1.1.0.0
{-# INLINE unsafeKeys #-}
unsafeKeys :: (PrimMonad m, VU.Unbox a) => HashMap (PrimState m) a -> m (VU.Vector Int)
unsafeKeys HashMap {..} = do
  used <- VU.unsafeFreeze usedHM
  keys_ <- VU.unsafeFreeze keyHM
  pure $ VU.ifilter (const . unBit . (used VG.!)) keys_

-- | \(O(n)\) Enumerates the elements (values) in the hash map.
--
-- @since 1.1.0.0
{-# INLINE unsafeElems #-}
unsafeElems :: (PrimMonad m, VU.Unbox a) => HashMap (PrimState m) a -> m (VU.Vector a)
unsafeElems HashMap {..} = do
  used <- VU.unsafeFreeze usedHM
  vals <- VU.unsafeFreeze valHM
  pure $ VU.ifilter (const . unBit . (used VG.!)) vals

-- | \(O(n)\) Enumerates the key-value pairs in the hash map.
--
-- @since 1.1.0.0
{-# INLINE unsafeAssocs #-}
unsafeAssocs :: (PrimMonad m, VU.Unbox a) => HashMap (PrimState m) a -> m (VU.Vector (Int, a))
unsafeAssocs HashMap {..} = do
  used <- VU.unsafeFreeze usedHM
  keys_ <- VU.unsafeFreeze keyHM
  vals <- VU.unsafeFreeze valHM
  pure $ VU.ifilter (const . unBit . (used VG.!)) $ VU.zip keys_ vals
