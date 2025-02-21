{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK hide #-}

-- | Base module of a dynamic, sparse segment tree.
--
-- @since 1.2.1.0
module AtCoder.Extra.DynSparseSegTree.Raw
  ( -- * Dynamic, sparse segment tree
    DynSparseSegTree (..),

    -- * Re-exports
    P.Index (..),

    -- * Constructors
    newST,
    newRootST,
    newNodeST,
    freeSubtreeST,

    -- * Accessing elements
    modifyMST,

    -- * Products
    prodST,
    -- prodMaybe,

    -- * Binary searches
    maxRightM,
    -- -- * Conversions
    -- freezeST,
  )
where

import AtCoder.Extra.Pool qualified as P
import AtCoder.Internal.Assert qualified as ACIA
import Control.Monad (unless)
import Control.Monad.Primitive (PrimMonad, PrimState, stToPrim)
import Control.Monad.ST (ST)
import Data.Coerce (coerce)
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Stack (HasCallStack)
import Prelude hiding (read)

-- | A dynamic, sparse segment tree that covers a half-open interval \([l_0, r_0)\). Is is dynamic
-- in that the nodes are instantinated as needed.
--
-- @since 1.2.1.0
data DynSparseSegTree s a = DynSparseSegTree
  { -- | The maximum number of nodes allocated
    --
    -- @since 1.2.1.0
    capacityDsst :: {-# UNPACK #-} !Int,
    -- | Whether the data is persistent or not
    --
    -- @since 1.2.1.0
    isPersistentDsst :: {-# UNPACK #-} !Bool,
    -- | Left index boundary (inclusive)
    --
    -- @since 1.2.1.0
    l0Dsst :: {-# UNPACK #-} !Int,
    -- | Right index boundary (exclusive)
    --
    -- @since 1.2.1.0
    r0Dsst :: {-# UNPACK #-} !Int,
    -- | `Pool` for free slot management.
    --
    -- @since 1.2.1.0
    poolDsst :: !(P.Pool s ()),
    -- | Decomposed node storage: left children
    --
    -- @since 1.2.1.0
    lDsst :: !(VUM.MVector s P.Index),
    -- | Decomposed node storage: right children
    --
    -- @since 1.2.1.0
    rDsst :: !(VUM.MVector s P.Index),
    -- | Decomposed node storage: position
    --
    -- @since 1.2.1.0
    xDsst :: !(VUM.MVector s a),
    -- | Decomposed node storage: position
    --
    -- @since 1.2.1.0
    iDsst :: !(VUM.MVector s Int),
    -- | Decomposed node storage: monoid product
    --
    -- @since 1.2.1.0
    prodDsst :: !(VUM.MVector s a)
  }

-- | \(O(n)\)
--
-- @since 1.2.1.0
{-# INLINEABLE newST #-}
newST :: (HasCallStack, VU.Unbox a) => Bool -> Int -> Int -> Int -> ST s (DynSparseSegTree s a)
newST isPersistentDsst capacityDsst l0Dsst r0Dsst = do
  let !_ = ACIA.runtimeAssert (l0Dsst <= r0Dsst) $ "AtCoder.Extra.DynSparseSegTree.Raw.newST: given invalid interval " ++ show (l0Dsst, r0Dsst)
  poolDsst <- P.new capacityDsst
  lDsst <- VUM.unsafeNew capacityDsst
  rDsst <- VUM.unsafeNew capacityDsst
  xDsst <- VUM.unsafeNew capacityDsst
  iDsst <- VUM.unsafeNew capacityDsst
  prodDsst <- VUM.unsafeNew capacityDsst
  pure DynSparseSegTree {..}

-- | \(O(1)\)
--
-- @since 1.2.1.0
{-# INLINE newRootST #-}
newRootST :: (HasCallStack, Monoid a, VU.Unbox a) => DynSparseSegTree s a -> ST s P.Index
newRootST _ = do
  pure P.undefIndex

-- | \(O(1)\)
--
-- @since 1.2.1.0
{-# INLINE newNodeST #-}
newNodeST :: (HasCallStack, Monoid a, VU.Unbox a) => DynSparseSegTree s a -> Int -> a -> ST s P.Index
newNodeST DynSparseSegTree {..} idx !x = do
  i <- P.alloc poolDsst ()
  VGM.write lDsst (coerce i) P.undefIndex
  VGM.write rDsst (coerce i) P.undefIndex
  VGM.write xDsst (coerce i) x
  VGM.write iDsst (coerce i) idx
  VGM.write prodDsst (coerce i) x
  pure i

-- | \(O(n)\)
--
-- @since 1.2.1.0
{-# INLINE freeSubtreeST #-}
freeSubtreeST :: (HasCallStack, Monoid a, VU.Unbox a) => DynSparseSegTree s a -> P.Index -> ST s ()
freeSubtreeST DynSparseSegTree {..} i = do
  let inner c = do
        cl <- VGM.read lDsst (coerce c)
        cr <- VGM.read rDsst (coerce c)
        unless (P.nullIndex cl) $ P.free poolDsst cl
        unless (P.nullIndex cr) $ P.free poolDsst cr
  inner i

-- | \(O(\log L)\)
--
-- @since 1.2.1.0
{-# INLINEABLE modifyMST #-}
modifyMST :: forall m a. (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => DynSparseSegTree (PrimState m) a -> P.Index -> (a -> m a) -> Int -> m P.Index
modifyMST dst@DynSparseSegTree {..} root f i0
  | P.nullIndex root = stToPrim . newNodeST dst i0 =<< f mempty
  | otherwise = inner root l0Dsst r0Dsst i0
  where
    !_ = ACIA.checkIndexBounded "AtCoder.Extra.DynSparseSegTree.Raw.modifyMST" i0 l0Dsst r0Dsst
    inner :: P.Index -> Int -> Int -> Int -> m P.Index
    inner c_ l r i
      | P.nullIndex c_ = stToPrim . newNodeST dst i =<< f mempty
      | otherwise = do
          c <- stToPrim $ cloneOnWriteST dst c_
          ci <- stToPrim $ VGM.read iDsst (coerce c)
          if ci == i
            then do
              VGM.modifyM xDsst f (coerce c)
              stToPrim $ updateNodeST dst c
              pure c
            else do
              -- update left or right child
              let m = (l + r) `div` 2
              if i < m
                then do
                  cl <- stToPrim $ VGM.read lDsst (coerce c)
                  if ci < i
                    then do
                      -- TODO: is this statement correct?
                      -- now we know `i` does not exist in the segment tree.
                      stToPrim $ VGM.write iDsst (coerce c) i
                      cx <- stToPrim . VGM.exchange xDsst (coerce c) =<< f mempty
                      -- re-allocate `c` as a child:
                      stToPrim . VGM.write lDsst (coerce c) =<< inner2 cl l m ci cx
                    else do
                      stToPrim . VGM.write lDsst (coerce c) =<< inner cl l m i
                else do
                  cr <- stToPrim $ VGM.read rDsst (coerce c)
                  if i < ci
                    then do
                      -- now we know `i` does not exist in the segment tree.
                      stToPrim $ VGM.write iDsst (coerce c) i
                      cx <- stToPrim . VGM.exchange xDsst (coerce c) =<< f mempty
                      -- re-allocate `c` as a child:
                      stToPrim . VGM.write rDsst (coerce c) =<< inner2 cr m r ci cx
                    else do
                      stToPrim . VGM.write rDsst (coerce c) =<< inner cr m r i

              stToPrim $ updateNodeST dst c
              pure c

    -- insert new node
    inner2 :: P.Index -> Int -> Int -> Int -> a -> m P.Index
    inner2 c_ l r i x
      | P.nullIndex c_ = stToPrim $ newNodeST dst i x
      | otherwise = do
          c <- stToPrim $ cloneOnWriteST dst c_
          ci <- stToPrim $ VGM.read iDsst (coerce c)
          let !_ = ACIA.runtimeAssert (ci /= i) "AtCoder.Extra.DynSparseSegTree.Raw.modifyMST: wrong implementation"
          -- update left or right child
          let m = (l + r) `div` 2
          if i < m
            then do
              cl <- stToPrim $ VGM.read lDsst (coerce c)
              if ci < i
                then do
                  stToPrim $ VGM.write iDsst (coerce c) i
                  cx <- stToPrim $ VGM.exchange xDsst (coerce c) x
                  stToPrim . VGM.write lDsst (coerce c) =<< inner2 cl l m ci cx
                else do
                  stToPrim . VGM.write lDsst (coerce c) =<< inner2 cl l m i x
            else do
              cr <- stToPrim $ VGM.read rDsst (coerce c)
              if i < ci
                then do
                  stToPrim $ VGM.write iDsst (coerce c) i
                  cx <- stToPrim $ VGM.exchange xDsst (coerce c) x
                  stToPrim . VGM.write rDsst (coerce c) =<< inner2 cr m r ci cx
                else do
                  stToPrim . VGM.write rDsst (coerce c) =<< inner2 cr m r i x

          stToPrim $ updateNodeST dst c
          pure c

-- | \(O(\log L)\)
--
-- @since 1.2.1.0
{-# INLINEABLE prodST #-}
prodST :: forall a s. (HasCallStack, Monoid a, VU.Unbox a) => DynSparseSegTree s a -> P.Index -> Int -> Int -> ST s a
prodST DynSparseSegTree {..} root0 ql0 qr0
  | ql0 >= qr0 || P.nullIndex root0 = pure mempty
  | otherwise = inner root0 l0Dsst r0Dsst ql0 qr0 mempty
  where
    !_ = ACIA.checkIntervalBounded "AtCoder.Extra.DynSparseSegTree.Raw.prodST" ql0 qr0 l0Dsst r0Dsst
    -- left to right
    -- - l, r: node interval
    -- - ql, qr: queried interval
    inner :: P.Index -> Int -> Int -> Int -> Int -> a -> ST s a
    inner c l r ql_ qr_ !x
      | len <= 0 = pure x
      | P.nullIndex c = pure x
      | l == ql && r == qr = do
          cProd <- VGM.read prodDsst (coerce c)
          pure $! x <> cProd
      | otherwise = do
          let m = (l + r) `div` 2
          cl <- VGM.read lDsst (coerce c)
          x' <- inner cl l m ql qr x
          ci <- VGM.read iDsst (coerce c)
          x'' <- if ql <= ci && ci < qr then (x' <>) <$> VGM.read xDsst (coerce c) else pure x'
          cr <- VGM.read rDsst (coerce c)
          inner cr m r ql qr x''
      where
        -- shrink target interval to node interval
        ql = max ql_ l
        qr = min qr_ r
        len = qr - ql

-- | \(O(\log L)\)
--
-- ==== Constraints
-- - The segment tree is not empty
-- - User predicate \(f\) returns `True` for `mempty`
--
-- @since 1.2.1.0
{-# INLINEABLE maxRightM #-}
maxRightM :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => DynSparseSegTree (PrimState m) a -> P.Index -> (a -> m Bool) -> m Int
maxRightM DynSparseSegTree {..} root f = do
  (!r, !_) <- inner root l0Dsst r0Dsst mempty
  pure r
  where
    -- returning `r0Dsst` means "go right"
    inner c l r !xParent
      | P.nullIndex c = pure (r0Dsst, xParent)
      | otherwise = do
          xWhole <- stToPrim $ (xParent <>) <$> VGM.read prodDsst (coerce c)
          b <- f xWhole
          if b
            then do
              pure (r0Dsst, xWhole)
            else do
              let m = (l + r) `div` 2
              cl <- stToPrim $ VGM.read lDsst (coerce c)
              (!k, !xl) <- inner cl l m xParent
              if k /= r0Dsst
                then pure (k, xl)
                else do
                  xm <- stToPrim $ (xl <>) <$> VGM.read xDsst (coerce c)
                  b' <- f xm
                  if not b'
                    then stToPrim $ (,xm) <$> VGM.read iDsst (coerce c)
                    else do
                      cr <- stToPrim $ VGM.read rDsst (coerce c)
                      inner cr m r xm

-- -------------------------------------------------------------------------------------------------
-- Internals
-- -------------------------------------------------------------------------------------------------

-- | \(O(1)\) Optionally clones a node depending on the persistency setting.
{-# INLINEABLE cloneOnWriteST #-}
cloneOnWriteST :: (HasCallStack, Monoid a, VU.Unbox a) => DynSparseSegTree s a -> P.Index -> ST s P.Index
cloneOnWriteST DynSparseSegTree {..} c
  | not isPersistentDsst || P.nullIndex c = pure c
  | otherwise = do
      i <- P.alloc poolDsst ()
      VGM.write lDsst (coerce i) =<< VGM.read lDsst (coerce c)
      VGM.write rDsst (coerce i) =<< VGM.read rDsst (coerce c)
      VGM.write xDsst (coerce i) =<< VGM.read xDsst (coerce c)
      VGM.write prodDsst (coerce i) =<< VGM.read prodDsst (coerce c)
      VGM.write iDsst (coerce i) =<< VGM.read iDsst (coerce c)
      pure i

-- | \(O(1)\)
{-# INLINEABLE updateNodeST #-}
updateNodeST :: (HasCallStack, Monoid a, VU.Unbox a) => DynSparseSegTree s a -> P.Index -> ST s ()
updateNodeST DynSparseSegTree {..} c = do
  VGM.write prodDsst (coerce c) =<< VGM.read xDsst (coerce c)
  cl <- VGM.read lDsst (coerce c)
  unless (P.nullIndex cl) $ do
    prodL <- VGM.read prodDsst (coerce cl)
    VGM.modify prodDsst (prodL <>) (coerce c)
  cr <- VGM.read rDsst (coerce c)
  unless (P.nullIndex cr) $ do
    prodR <- VGM.read prodDsst (coerce cr)
    VGM.modify prodDsst (<> prodR) (coerce c)
