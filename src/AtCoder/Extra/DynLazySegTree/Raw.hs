{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- | Base module of a dynamic, lazily propagated segment tree.
--
-- @since 1.2.1.0
module AtCoder.Extra.DynLazySegTree.Raw
  ( -- * Dynamic, lazily propagated segment tree
    DynLazySegTree (..),

    -- * Re-exports
    SegAct (..),
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

    -- * Applications
    applyInST,

    -- * Tree operations
    copyIntervalWithST,
    resetIntervalST,

    -- * Binary searches
    maxRightM,
    -- -- * Conversions
    -- freezeST,
  )
where

import AtCoder.Extra.Pool qualified as P
import AtCoder.Internal.Assert qualified as ACIA
import AtCoder.LazySegTree (SegAct (..))
import Control.Monad (when)
import Control.Monad.Primitive (PrimMonad, PrimState, stToPrim)
import Control.Monad.ST (ST)
import Data.Coerce (coerce)
import Data.Maybe (fromMaybe)
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Stack (HasCallStack)
import Prelude hiding (read)

-- | A dynamic, lazily propagated segment tree that covers a half-open interval \([l_0, r_0)\).
-- The nodes are instantinated as needed.
--
-- @since 1.2.1.0
data DynLazySegTree s f a = DynLazySegTree
  { -- | The maximum number of nodes allocated
    --
    -- @since 1.2.1.0
    capacityLdst :: {-# UNPACK #-} !Int,
    -- | Whether the data is persistent or not
    --
    -- @since 1.2.1.0
    isPersistentLdst :: {-# UNPACK #-} !Bool,
    -- | Left index boundary (inclusive)
    --
    -- @since 1.2.1.0
    l0Ldst :: {-# UNPACK #-} !Int,
    -- | Right index boundary (exclusive)
    --
    -- @since 1.2.1.0
    r0Ldst :: {-# UNPACK #-} !Int,
    -- | Initial monoid value assignment \(g: (l, r) \rightarrow a\)
    --
    -- @since 1.2.1.0
    initialProdLdst :: !(Int -> Int -> a),
    -- | `Pool` for free slot management.
    --
    -- @since 1.2.1.0
    poolLdst :: !(P.Pool s ()),
    -- | Decomposed node storage: left children
    --
    -- @since 1.2.1.0
    lLdst :: !(VUM.MVector s P.Index),
    -- | Decomposed node storage: right children
    --
    -- @since 1.2.1.0
    rLdst :: !(VUM.MVector s P.Index),
    -- | Decomposed node storage: monoid value
    --
    -- @since 1.2.1.0
    xLdst :: !(VUM.MVector s a),
    -- | Decomposed node storage: lazily propagated monoid action
    --
    -- @since 1.2.1.0
    lazyLdst :: !(VUM.MVector s f)
  }

-- | \(O(n)\)
--
-- @since 1.2.1.0
{-# INLINEABLE newST #-}
newST :: (HasCallStack, VU.Unbox f, VU.Unbox a) => Bool -> Int -> Int -> Int -> (Int -> Int -> a) -> ST s (DynLazySegTree s f a)
newST isPersistentLdst capacityLdst l0Ldst r0Ldst initialProdLdst = do
  let !_ = ACIA.runtimeAssert (l0Ldst <= r0Ldst) $ "AtCoder.Extra.DynLazySegTree.Raw.newST: given invalid interval " ++ show (l0Ldst, r0Ldst)
  poolLdst <- P.new capacityLdst
  lLdst <- VUM.unsafeNew capacityLdst
  rLdst <- VUM.unsafeNew capacityLdst
  xLdst <- VUM.unsafeNew capacityLdst
  lazyLdst <- VUM.unsafeNew capacityLdst
  pure DynLazySegTree {..}

-- | \(O(1)\)
--
-- @since 1.2.1.0
{-# INLINE newRootST #-}
newRootST :: (HasCallStack, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => DynLazySegTree s f a -> ST s P.Index
newRootST dst@DynLazySegTree {..} = do
  newNodeInST dst l0Ldst r0Ldst

-- | \(O(1)\)
--
-- @since 1.2.1.0
{-# INLINE newNodeST #-}
newNodeST :: (HasCallStack, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => DynLazySegTree s f a -> a -> ST s P.Index
newNodeST DynLazySegTree {..} !x = do
  i <- P.alloc poolLdst ()
  VGM.write lLdst (coerce i) P.undefIndex
  VGM.write rLdst (coerce i) P.undefIndex
  VGM.write xLdst (coerce i) x
  VGM.write lazyLdst (coerce i) mempty
  pure i

-- | \(O(L)\)
--
-- @since 1.2.1.0
{-# INLINEABLE newSeqST #-}
newSeqST :: (HasCallStack, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => DynLazySegTree s f a -> VU.Vector a -> ST s P.Index
newSeqST dst@DynLazySegTree {..} !xs = do
  let !_ = ACIA.runtimeAssert (l0Ldst == 0 && r0Ldst == VU.length xs) "AtCoder.Extra.DynLazySegTree.Raw: mismatch between the bounds and the input vector: the bounds must be [0, n)"
  -- run DFS and allocate nodes from left to right
  let dfs l r
        | l == r = pure P.undefIndex
        | r - l == 1 = newNodeST dst $ xs VG.! l
        | otherwise = do
            let m = (l + r) `div` 2
            lRoot <- dfs l m
            rRoot <- dfs m r
            xlRoot <- VGM.read xLdst (coerce lRoot)
            xrRoot <- VGM.read xLdst (coerce rRoot)
            let !x = xlRoot <> xrRoot
            root <- newNodeST dst x
            VGM.write lLdst (coerce root) lRoot
            VGM.write rLdst (coerce root) rRoot
            pure root
  dfs 0 (VU.length xs)

-- | \(O(1)\)
--
-- ==== Constraints
-- - The interval must be non-empty
--
-- @since 1.2.1.0
{-# INLINE newNodeInST #-}
newNodeInST :: (HasCallStack, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => DynLazySegTree s f a -> Int -> Int -> ST s P.Index
newNodeInST dst@DynLazySegTree {initialProdLdst} l r = do
  let !_ = ACIA.runtimeAssert (r > l) $ "AtCoder.Extra.DynLazySegTree.Raw.nodeNodeInST: not empty or negative interval: " ++ show (l, r)
  newNodeST dst $! initialProdLdst l r

-- | \(O(\log L)\)
--
-- @since 1.2.1.0
{-# INLINEABLE modifyMST #-}
modifyMST :: forall m f a. (HasCallStack, PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => DynLazySegTree (PrimState m) f a -> P.Index -> (a -> m a) -> Int -> m P.Index
modifyMST dst@DynLazySegTree {..} root f i = inner root l0Ldst r0Ldst
  where
    !_ = ACIA.checkIndexBounded "AtCoder.Extra.DynLazySegTree.Raw.modifyMST" i l0Ldst r0Ldst
    inner :: P.Index -> Int -> Int -> m P.Index
    inner c l r
      | r - l == 1 = do
          -- let !_ = ACIA.runtimeAssert (i == l) ""
          c' <- stToPrim $ cloneOnWriteST dst c
          VGM.modifyM xLdst f (coerce c')
          stToPrim $ VGM.write lazyLdst (coerce c') mempty
          pure c'
      | otherwise = do
          stToPrim $ propST dst c l r
          let m = (l + r) `div` 2

          -- lazily allocate left and right children:
          cl <- stToPrim $ do
            j <- VGM.read lLdst (coerce c)
            if P.nullIndex j
              then do
                j' <- newNodeInST dst l m
                VGM.write lLdst (coerce c) j'
                pure j'
              else pure j

          cr <- stToPrim $ do
            j <- VGM.read rLdst (coerce c)
            if P.nullIndex j
              then do
                j' <- newNodeInST dst m r
                VGM.write rLdst (coerce c) j'
                pure j'
              else pure j

          c' <- stToPrim $ cloneOnWriteST dst c
          if i < m
            then stToPrim . VGM.write lLdst (coerce c') =<< inner cl l m
            else stToPrim . VGM.write rLdst (coerce c') =<< inner cr m r

          stToPrim $ do
            -- Note that either left or right child of `c'` is updated in the above `if`
            clx <- VGM.read xLdst . coerce =<< VGM.read lLdst (coerce c')
            crx <- VGM.read xLdst . coerce =<< VGM.read rLdst (coerce c')
            VGM.write xLdst (coerce c') $! clx <> crx
          pure c'

-- | \(O(\log L)\)
--
-- @since 1.2.1.0
{-# INLINEABLE applyInST #-}
applyInST :: forall f a s. (HasCallStack, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => DynLazySegTree s f a -> P.Index -> Int -> Int -> f -> ST s P.Index
applyInST dst@DynLazySegTree {..} root0 ql0 qr0 !f
  | ql0 == qr0 = pure root0
  | otherwise = inner root0 l0Ldst r0Ldst ql0 qr0
  where
    !_ = ACIA.checkIntervalBounded "AtCoder.Extra.DynLazySegTree.Raw.applyInST" ql0 qr0 l0Ldst r0Ldst
    !_ = ACIA.runtimeAssert (not (P.nullIndex root0)) "AtCoder.Extra.DynLazySegTree.Raw.applyInST"
    -- left to right
    -- - l, r: node interval
    -- - ql, qr: queried interval
    inner :: P.Index -> Int -> Int -> Int -> Int -> ST s P.Index
    inner c_ l r ql_ qr_ = do
      c <- if P.nullIndex c_ then newNodeInST dst l r else pure c_
      if len <= 0
        then do
          -- A node of length zero would be created if the interval length is odd. It's OK:
          pure c
        else
          if l == ql && r == qr
            then do
              c' <- cloneOnWriteST dst c
              VGM.modify xLdst (segActWithLength len f) (coerce c')
              VGM.modify lazyLdst (f <>) (coerce c')
              pure c'
            else do
              propST dst c l r
              let m = (l + r) `div` 2
              c' <- cloneOnWriteST dst c
              VGM.modifyM lLdst (\i -> inner i l m ql qr) (coerce c')
              VGM.modifyM rLdst (\i -> inner i m r ql qr) (coerce c')
              clx <- VGM.read xLdst . coerce =<< VGM.read lLdst (coerce c')
              crx <- VGM.read xLdst . coerce =<< VGM.read rLdst (coerce c')
              VGM.write xLdst (coerce c') $! clx <> crx
              pure c'
      where
        -- shrink target interval to node interval
        ql = max ql_ l
        qr = min qr_ r
        len = qr - ql

-- | \(O(\log L)\)
--
-- @since 1.2.1.0
{-# INLINEABLE prodST #-}
prodST :: forall f a s. (HasCallStack, SegAct f a, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => DynLazySegTree s f a -> P.Index -> Int -> Int -> ST s a
prodST DynLazySegTree {..} root0 ql0 qr0
  | ql0 >= qr0 || P.nullIndex root0 = pure mempty
  | otherwise = inner root0 l0Ldst r0Ldst ql0 qr0 mempty mempty
  where
    !_ = ACIA.checkIntervalBounded "AtCoder.Extra.DynLazySegTree.Raw.prodST" ql0 qr0 l0Ldst r0Ldst
    -- left to right
    -- - l, r: node interval
    -- - ql, qr: queried interval
    inner :: P.Index -> Int -> Int -> Int -> Int -> a -> f -> ST s a
    inner c l r ql_ qr_ !x !f
      | len <= 0 = pure x
      | P.nullIndex c = do
          pure $! x <> segActWithLength len f (initialProdLdst ql qr)
      | l == ql && r == qr = do
          cx <- VGM.read xLdst (coerce c)
          pure $! x <> segActWithLength len f cx
      | otherwise = do
          let m = (l + r) `div` 2
          !f' <- (f <>) <$> VGM.read lazyLdst (coerce c)
          cl <- VGM.read lLdst (coerce c)
          x' <- inner cl l m ql qr x f'
          cr <- VGM.read rLdst (coerce c)
          x'' <- inner cr m r ql qr x' f'
          pure x''
      where
        -- shrink target interval to node interval
        ql = max ql_ l
        qr = min qr_ r
        len = qr - ql

-- | \(O(\log L)\)
--
-- @since 1.2.1.0
{-# INLINEABLE copyIntervalWithST #-}
copyIntervalWithST ::
  forall f a s.
  (HasCallStack, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) =>
  -- | Dynamic segment tree
  DynLazySegTree s f a ->
  -- | Root to be modified
  P.Index ->
  -- | Another segment tree
  P.Index ->
  -- | \(l\)
  Int ->
  -- | \(r\)
  Int ->
  -- | Action \(f\)
  f ->
  -- | New root
  ST s P.Index
copyIntervalWithST dst@DynLazySegTree {..} rootA rootB ql0 qr0 !f0
  | rootA == rootB = pure rootA
  | otherwise = do
      if ql0 >= qr0
        then pure rootA
        else do
          rootA' <- cloneOnWriteST dst rootA
          inner rootA' rootB l0Ldst r0Ldst ql0 qr0 f0
          pure rootA'
  where
    !_ = ACIA.checkIntervalBounded "AtCoder.Extra.DynLazySegTree.Raw.copyIntervalWithST" ql0 qr0 l0Ldst r0Ldst
    !_ = ACIA.runtimeAssert (not (P.nullIndex rootA)) "AtCoder.Extra.DynLazySegTree.Raw.copyIntervalWithST: given null"

    -- left to right
    -- - l, r: node interval
    -- - ql, qr: queried interval
    inner :: P.Index -> P.Index -> Int -> Int -> Int -> Int -> f -> ST s ()
    inner c d l r ql_ qr_ !f
      | len <= 0 = pure () -- `c` is already cloned
      | l == ql && r == qr = do
          if not (P.nullIndex d)
            then do
              VGM.write xLdst (coerce c) . segActWithLength len f =<< VGM.read xLdst (coerce d)
              VGM.write lazyLdst (coerce c) . (f <>) =<< VGM.read lazyLdst (coerce d)
              VGM.write lLdst (coerce c) =<< VGM.read lLdst (coerce d)
              VGM.write rLdst (coerce c) =<< VGM.read rLdst (coerce d)
            else do
              VGM.write xLdst (coerce c) . segActWithLength len f $! initialProdLdst l r
              VGM.write lazyLdst (coerce c) f
              VGM.write lLdst (coerce c) P.undefIndex
              VGM.write rLdst (coerce c) P.undefIndex
      | otherwise = do
          let m = (l + r) `div` 2
          VGM.modifyM lLdst (\i -> if P.nullIndex i then newNodeST dst (initialProdLdst l m) else cloneOnWriteST dst i) (coerce c)
          cl <- VGM.read lLdst (coerce c)
          VGM.modifyM rLdst (\i -> if P.nullIndex i then newNodeST dst (initialProdLdst m r) else cloneOnWriteST dst i) (coerce c)
          cr <- VGM.read rLdst (coerce c)
          cLazy <- VGM.exchange lazyLdst (coerce c) mempty
          VGM.modify xLdst (segActWithLength (m - l) cLazy) (coerce cl)
          VGM.modify xLdst (segActWithLength (r - m) cLazy) (coerce cr)
          VGM.modify lazyLdst (cLazy <>) (coerce cl)
          VGM.modify lazyLdst (cLazy <>) (coerce cr)
          !f' <- if P.nullIndex d then pure f else (f <>) <$> VGM.read lazyLdst (coerce d)
          dl' <- fromMaybe P.undefIndex <$> VGM.readMaybe lLdst (coerce d)
          dr' <- fromMaybe P.undefIndex <$> VGM.readMaybe rLdst (coerce d)
          inner cl dl' l m ql qr f'
          inner cr dr' m r ql qr f'
          clx <- VGM.read xLdst (coerce cl)
          crx <- VGM.read xLdst (coerce cr)
          VGM.write xLdst (coerce c) $! clx <> crx
      where
        !_ = ACIA.runtimeAssert (not (P.nullIndex c)) "AtCoder.Extra.DynLazySegTree.Raw.copyIntervalWithST: implementation error"
        -- shrink target interval to node interval
        ql = max ql_ l
        qr = min qr_ r
        len = qr - ql

-- | \(O(\log L)\) Resets an interval \([l, r)\) to initial monoid values.
--
-- @since 1.2.1.0
{-# INLINEABLE resetIntervalST #-}
resetIntervalST ::
  forall f a s.
  (HasCallStack, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) =>
  DynLazySegTree s f a ->
  P.Index ->
  Int ->
  Int ->
  ST s P.Index
resetIntervalST dst@DynLazySegTree {..} root ql0 qr0
  | ql0 == qr0 = pure root
  | P.nullIndex root = pure P.undefIndex
  | ql0 == l0Ldst && qr0 == r0Ldst = do
      -- for the case of non-persistent segment tere, we should update the root in-place:
      root' <- cloneOnWriteST dst root
      VGM.write xLdst (coerce root') $! initialProdLdst l0Ldst r0Ldst
      VGM.write lLdst (coerce root') P.undefIndex
      VGM.write rLdst (coerce root') P.undefIndex
      VGM.write lazyLdst (coerce root') mempty
      pure root'
  | otherwise = inner root l0Ldst r0Ldst ql0 qr0
  where
    !_ = ACIA.checkIntervalBounded "AtCoder.Extra.DynLazySegTree.Raw.resetIntervalST" ql0 qr0 l0Ldst r0Ldst

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
          propST dst c l r
          let m = (l + r) `div` 2
          c' <- cloneOnWriteST dst c
          VGM.modifyM lLdst (\i -> inner i l m ql qr) (coerce c')
          cl <- VGM.read lLdst (coerce c')
          VGM.modifyM rLdst (\i -> inner i m r ql qr) (coerce c')
          cr <- VGM.read rLdst (coerce c')
          clx <- if P.nullIndex cl then pure $! initialProdLdst l m else VGM.read xLdst (coerce cl)
          crx <- if P.nullIndex cr then pure $! initialProdLdst m r else VGM.read xLdst (coerce cr)
          VGM.write xLdst (coerce c') $! clx <> crx
          pure c'
      where
        -- shrink target interval to node interval
        ql = max ql_ l
        qr = min qr_ r
        len = qr - ql

-- It returns only allocated leaf values
-- {-# INLINEABLE freezeST #-}
-- freezeST :: forall f a s. (HasCallStack, SegAct f a, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => DynLazySegTree s f a -> P.Index -> ST s (VU.Vector a)
-- freezeST DynLazySegTree {..} root = do
--   buf <- B.new (r0Ldst - l0Ldst)
--
--   let inner c l r !f
--         | P.nullIndex c = pure ()
--         | r - l == 1 = do
--             x <- VGM.read xLdst (coerce c)
--             B.pushBack buf $! segAct f x
--         | otherwise = do
--             let m = (l + r) `div` 2
--             !f' <- (f <>) <$> VGM.read lazyLdst (coerce c)
--             cl <- VGM.read lLdst (coerce c)
--             cr <- VGM.read rLdst (coerce c)
--             inner cl l m f'
--             inner cr m r f'
--
--   inner root l0Ldst r0Ldst mempty
--   B.unsafeFreeze buf

-- | \(O(\log L)\)
--
-- @since 1.2.1.0
{-# INLINEABLE maxRightM #-}
maxRightM :: (HasCallStack, PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => DynLazySegTree (PrimState m) f a -> P.Index -> (a -> m Bool) -> m Int
maxRightM dst@DynLazySegTree {..} root !f = do
  (!r, !_) <- inner root l0Ldst r0Ldst mempty
  pure r
  where
    -- FIXME: it should not allocate new nodes
    inner c_ l r !x = do
      c <- if P.nullIndex c_ then stToPrim $ newNodeInST dst l r else pure c_
      xWhole <- stToPrim $ (x <>) <$> VGM.read xLdst (coerce c)
      b <- f xWhole
      if b
        then do
          pure (r, xWhole)
        else do
          if r - l == 1
            then pure (l, x)
            else do
              stToPrim $ propST dst c l r
              let m = (l + r) `div` 2
              cl <- stToPrim $ VGM.read lLdst (coerce c)
              (!k, !xl) <- inner cl l m x
              if k < m
                then pure (k, xl)
                else do
                  cr <- stToPrim $ VGM.read rLdst (coerce c)
                  inner cr m r xl

-- -------------------------------------------------------------------------------------------------
-- Internals
-- -------------------------------------------------------------------------------------------------

-- | \(O(1)\) Optionally clones a node depending on the persistency setting.
{-# INLINEABLE cloneOnWriteST #-}
cloneOnWriteST :: (HasCallStack, SegAct f a, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => DynLazySegTree s f a -> P.Index -> ST s P.Index
cloneOnWriteST DynLazySegTree {..} c
  | not isPersistentLdst || P.nullIndex c = pure c
  | otherwise = do
      i <- P.alloc poolLdst ()
      VGM.write lLdst (coerce i) =<< VGM.read lLdst (coerce c)
      VGM.write rLdst (coerce i) =<< VGM.read rLdst (coerce c)
      VGM.write xLdst (coerce i) =<< VGM.read xLdst (coerce c)
      VGM.write lazyLdst (coerce i) =<< VGM.read lazyLdst (coerce c)
      pure i

-- | \(O(1)\)
{-# INLINEABLE propST #-}
propST :: (HasCallStack, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => DynLazySegTree s f a -> P.Index -> Int -> Int -> ST s ()
propST dst@DynLazySegTree {..} c l r = do
  let !_ = ACIA.runtimeAssert (r - l >= 2) "AtCoder.Extra.DynLazySegTree.Raw.propST: the interval must have length more than or equal to `2`"
  cLazy <- VGM.read lazyLdst (coerce c)
  when (cLazy /= mempty) $ do
    let m = (l + r) `div` 2

    -- create or clone left child
    cl <- do
      i <- VGM.read lLdst (coerce c)
      if P.nullIndex i
        then newNodeInST dst l m
        else cloneOnWriteST dst i
    VGM.write lLdst (coerce c) cl
    VGM.modify xLdst (segActWithLength (m - l) cLazy) (coerce cl)
    VGM.modify lazyLdst (cLazy <>) (coerce cl)

    -- create or clone right child
    cr <- do
      i <- VGM.read rLdst (coerce c)
      if P.nullIndex i
        then newNodeInST dst m r
        else cloneOnWriteST dst i
    VGM.write rLdst (coerce c) cr
    VGM.modify xLdst (segActWithLength (r - m) cLazy) (coerce cr)
    VGM.modify lazyLdst (cLazy <>) (coerce cr)

    -- clear the lazy action
    VGM.write lazyLdst (coerce c) mempty
