{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK hide #-}

-- | Base module of a dynamic segment tree.
--
-- @since 1.2.1.0
module AtCoder.Extra.DynSegTree.Raw
  ( -- * Dynamic segment tree
    DynSegTree (..),

    -- * Re-exports
    P.Index (..),

    -- * Constructors
    newST,
    newRootST,
    newNodeST,
    newSeqST,

    -- * Accessing elements
    modifyMST,

    -- * Products
    prodST,
    -- prodMaybe,

    -- * Tree operations
    resetIntervalST,

    -- * Binary searches
    maxRightM,
    -- -- * Conversions
    -- freezeST,
  )
where

import AtCoder.Extra.Pool qualified as P
import AtCoder.Internal.Assert qualified as ACIA
import Control.Monad.Primitive (PrimMonad, PrimState, stToPrim)
import Control.Monad.ST (ST)
import Data.Coerce (coerce)
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Stack (HasCallStack)
import Prelude hiding (read)

-- | A dynamic segment tree that covers a half-open interval \([l_0, r_0)\). Is is dynamic in that
-- the nodes are instantinated as needed.
--
-- @since 1.2.1.0
data DynSegTree s a = DynSegTree
  { -- | The maximum number of nodes allocated
    --
    -- @since 1.2.1.0
    capacityDst :: {-# UNPACK #-} !Int,
    -- | Whether the data is persistent or not
    --
    -- @since 1.2.1.0
    isPersistentDst :: {-# UNPACK #-} !Bool,
    -- | Left index boundary (inclusive)
    --
    -- @since 1.2.1.0
    l0Dst :: {-# UNPACK #-} !Int,
    -- | Right index boundary (exclusive)
    --
    -- @since 1.2.1.0
    r0Dst :: {-# UNPACK #-} !Int,
    -- | Initial monoid value assignment \(g: (l, r) \rightarrow a\)
    --
    -- @since 1.2.1.0
    initialProdDst :: !(Int -> Int -> a),
    -- | `Pool` for free slot management.
    --
    -- @since 1.2.1.0
    poolDst :: !(P.Pool s ()),
    -- | Decomposed node storage: left children
    --
    -- @since 1.2.1.0
    lDst :: !(VUM.MVector s P.Index),
    -- | Decomposed node storage: right children
    --
    -- @since 1.2.1.0
    rDst :: !(VUM.MVector s P.Index),
    -- | Decomposed node storage: monoid value
    --
    -- @since 1.2.1.0
    xDst :: !(VUM.MVector s a)
  }

-- | \(O(n)\)
--
-- @since 1.2.1.0
{-# INLINEABLE newST #-}
newST :: (HasCallStack, VU.Unbox a) => Bool -> Int -> Int -> Int -> (Int -> Int -> a) -> ST s (DynSegTree s a)
newST isPersistentDst capacityDst l0Dst r0Dst initialProdDst = do
  let !_ = ACIA.runtimeAssert (l0Dst <= r0Dst) $ "AtCoder.Extra.DynSegTree.Raw.newST: given invalid interval " ++ show (l0Dst, r0Dst)
  poolDst <- P.new capacityDst
  lDst <- VUM.unsafeNew capacityDst
  rDst <- VUM.unsafeNew capacityDst
  xDst <- VUM.unsafeNew capacityDst
  pure DynSegTree {..}

-- | \(O(1)\)
--
-- @since 1.2.1.0
{-# INLINE newRootST #-}
newRootST :: (HasCallStack, Monoid a, VU.Unbox a) => DynSegTree s a -> ST s P.Index
newRootST dst@DynSegTree {..} = do
  newNodeInST dst l0Dst r0Dst

-- | \(O(1)\)
--
-- @since 1.2.1.0
{-# INLINE newNodeST #-}
newNodeST :: (HasCallStack, Monoid a, VU.Unbox a) => DynSegTree s a -> a -> ST s P.Index
newNodeST DynSegTree {..} !x = do
  i <- P.alloc poolDst ()
  VGM.write lDst (coerce i) P.undefIndex
  VGM.write rDst (coerce i) P.undefIndex
  VGM.write xDst (coerce i) x
  pure i

-- | \(O(L)\)
--
-- @since 1.2.1.0
{-# INLINEABLE newSeqST #-}
newSeqST :: (HasCallStack, Monoid a, VU.Unbox a) => DynSegTree s a -> VU.Vector a -> ST s P.Index
newSeqST dst@DynSegTree {..} !xs = do
  let !_ = ACIA.runtimeAssert (l0Dst == 0 && r0Dst == VU.length xs) "AtCoder.Extra.DynSegTree.Raw: mismatch between the bounds and the input vector: the bounds must be [0, n)"
  -- run DFS and allocate nodes from left to right
  let dfs l r
        | l == r = pure P.undefIndex
        | r - l == 1 = newNodeST dst $ xs VG.! l
        | otherwise = do
            let m = (l + r) `div` 2
            lRoot <- dfs l m
            rRoot <- dfs m r
            xlRoot <- VGM.read xDst (coerce lRoot)
            xrRoot <- VGM.read xDst (coerce rRoot)
            let !x = xlRoot <> xrRoot
            root <- newNodeST dst x
            VGM.write lDst (coerce root) lRoot
            VGM.write rDst (coerce root) rRoot
            pure root
  dfs 0 (VU.length xs)

-- | \(O(1)\)
--
-- ==== Constraints
-- - The interval must be non-empty
--
-- @since 1.2.1.0
{-# INLINE newNodeInST #-}
newNodeInST :: (HasCallStack, Monoid a, VU.Unbox a) => DynSegTree s a -> Int -> Int -> ST s P.Index
newNodeInST dst@DynSegTree {initialProdDst} l r = do
  let !_ = ACIA.runtimeAssert (r > l) $ "AtCoder.Extra.DynSegTree.Raw.nodeNodeInST: not empty or negative interval: " ++ show (l, r)
  newNodeST dst $! initialProdDst l r

-- | \(O(\log L)\)
--
-- @since 1.2.1.0
{-# INLINEABLE modifyMST #-}
modifyMST :: forall m a. (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => DynSegTree (PrimState m) a -> P.Index -> (a -> m a) -> Int -> m P.Index
modifyMST dst@DynSegTree {..} root f i = do
  root' <- stToPrim $ if P.nullIndex root then newRootST dst else cloneOnWriteST dst root
  inner root' l0Dst r0Dst
  where
    !_ = ACIA.checkIndexBounded "AtCoder.Extra.DynSegTree.Raw.modifyMST" i l0Dst r0Dst
    -- `c` is already cloned or newly allocated
    inner :: P.Index -> Int -> Int -> m P.Index
    inner c l r
      | r - l == 1 = do
          -- let !_ = ACIA.runtimeAssert (i == l) ""
          VGM.modifyM xDst f (coerce c)
          pure c
      | otherwise = do
          let m = (l + r) `div` 2

          -- one of left or right child is updated, not both
          if l <= i && i < m
            then do
              cl <- stToPrim $ do
                j <- VGM.read lDst (coerce c)
                if P.nullIndex j
                  then newNodeInST dst l m
                  else cloneOnWriteST dst j
              stToPrim $ VGM.write lDst (coerce c) cl
              _ <- inner cl l m
              pure ()
            else do
              cr <- stToPrim $ do
                j <- VGM.read rDst (coerce c)
                if P.nullIndex j
                  then newNodeInST dst m r
                  else cloneOnWriteST dst j
              stToPrim $ VGM.write rDst (coerce c) cr
              _ <- inner cr m r
              pure ()

          stToPrim $ do
            cl <- VGM.read lDst (coerce c)
            clx <- if P.nullIndex cl then pure $! initialProdDst l m else VGM.read xDst (coerce cl)
            cr <- VGM.read rDst (coerce c)
            crx <- if P.nullIndex cr then pure $! initialProdDst m r else VGM.read xDst (coerce cr)
            VGM.write xDst (coerce c) $! clx <> crx
          pure c

-- | \(O(\log L)\)
--
-- @since 1.2.1.0
{-# INLINEABLE prodST #-}
prodST :: forall a s. (HasCallStack, Monoid a, VU.Unbox a) => DynSegTree s a -> P.Index -> Int -> Int -> ST s a
prodST DynSegTree {..} root0 ql0 qr0
  | ql0 >= qr0 || P.nullIndex root0 = pure mempty
  | otherwise = inner root0 l0Dst r0Dst ql0 qr0 mempty
  where
    !_ = ACIA.checkIntervalBounded "AtCoder.Extra.DynSegTree.Raw.prodST" ql0 qr0 l0Dst r0Dst
    -- left to right
    -- - l, r: node interval
    -- - ql, qr: queried interval
    inner :: P.Index -> Int -> Int -> Int -> Int -> a -> ST s a
    inner c l r ql_ qr_ !x
      | len <= 0 = pure x
      | P.nullIndex c = do
          pure $! x <> initialProdDst ql qr
      | l == ql && r == qr = do
          cx <- VGM.read xDst (coerce c)
          pure $! x <> cx
      | otherwise = do
          let m = (l + r) `div` 2
          cl <- VGM.read lDst (coerce c)
          x' <- inner cl l m ql qr x
          cr <- VGM.read rDst (coerce c)
          inner cr m r ql qr x'
      where
        -- shrink target interval to node interval
        ql = max ql_ l
        qr = min qr_ r
        len = qr - ql

-- | \(O(\log L)\) Resets an interval \([l, r)\) to initial monoid values.
--
-- @since 1.2.1.0
{-# INLINEABLE resetIntervalST #-}
resetIntervalST ::
  forall a s.
  (HasCallStack, Monoid a, VU.Unbox a) =>
  DynSegTree s a ->
  P.Index ->
  Int ->
  Int ->
  ST s P.Index
resetIntervalST dst@DynSegTree {..} root ql0 qr0
  | ql0 == qr0 = pure root
  | P.nullIndex root = pure P.undefIndex
  | ql0 == l0Dst && qr0 == r0Dst = do
      -- for the case of non-persistent segment tere, we should update the root in-place:
      root' <- cloneOnWriteST dst root
      VGM.write xDst (coerce root') $! initialProdDst l0Dst r0Dst
      VGM.write lDst (coerce root') P.undefIndex
      VGM.write rDst (coerce root') P.undefIndex
      pure root'
  | otherwise = inner root l0Dst r0Dst ql0 qr0
  where
    !_ = ACIA.checkIntervalBounded "AtCoder.Extra.DynSegTree.Raw.resetIntervalST" ql0 qr0 l0Dst r0Dst

    -- replace interval with null
    inner :: P.Index -> Int -> Int -> Int -> Int -> ST s P.Index
    inner c l r ql_ qr_
      -- TODO: shall we allocate new node?
      | len <= 0 = pure c
      | P.nullIndex c = pure P.undefIndex
      -- NOTE: we're returning `undefIndex`, but we can instead free the subtree if it's not persistent
      | ql <= l && r <= qr = pure P.undefIndex
      | r - l == 1 = pure c
      | otherwise = do
          let m = (l + r) `div` 2
          c' <- cloneOnWriteST dst c
          VGM.modifyM lDst (\i -> inner i l m ql qr) (coerce c')
          cl <- VGM.read lDst (coerce c')
          VGM.modifyM rDst (\i -> inner i m r ql qr) (coerce c')
          cr <- VGM.read rDst (coerce c')
          clx <- if P.nullIndex cl then pure $! initialProdDst l m else VGM.read xDst (coerce cl)
          crx <- if P.nullIndex cr then pure $! initialProdDst m r else VGM.read xDst (coerce cr)
          VGM.write xDst (coerce c') $! clx <> crx
          pure c'
      where
        -- shrink target interval to node interval
        ql = max ql_ l
        qr = min qr_ r
        len = qr - ql

-- | \(O(\log L)\)
--
-- @since 1.2.1.0
{-# INLINEABLE maxRightM #-}
maxRightM :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => DynSegTree (PrimState m) a -> P.Index -> (a -> m Bool) -> m Int
maxRightM dst@DynSegTree {..} root f = do
  (!r, !_) <- inner root l0Dst r0Dst mempty
  pure r
  where
    -- FIXME: it should not allocate new nodes
    inner c_ l r !x = do
      c <- if P.nullIndex c_ then stToPrim $ newNodeInST dst l r else pure c_
      xWhole <- stToPrim $ (x <>) <$> VGM.read xDst (coerce c)
      b <- f xWhole
      if b
        then do
          pure (r, xWhole)
        else do
          if r - l == 1
            then pure (l, x)
            else do
              let m = (l + r) `div` 2
              cl <- stToPrim $ VGM.read lDst (coerce c)
              (!k, !xl) <- inner cl l m x
              if k < m
                then pure (k, xl)
                else do
                  cr <- stToPrim $ VGM.read rDst (coerce c)
                  inner cr m r xl

-- -------------------------------------------------------------------------------------------------
-- Internals
-- -------------------------------------------------------------------------------------------------

-- | \(O(1)\) Optionally clones a node depending on the persistency setting.
{-# INLINEABLE cloneOnWriteST #-}
cloneOnWriteST :: (HasCallStack, Monoid a, VU.Unbox a) => DynSegTree s a -> P.Index -> ST s P.Index
cloneOnWriteST DynSegTree {..} c
  | not isPersistentDst || P.nullIndex c = pure c
  | otherwise = do
      i <- P.alloc poolDst ()
      VGM.write lDst (coerce i) =<< VGM.read lDst (coerce c)
      VGM.write rDst (coerce i) =<< VGM.read rDst (coerce c)
      VGM.write xDst (coerce i) =<< VGM.read xDst (coerce c)
      pure i
