{-# LANGUAGE RecordWildCards #-}

-- | A lazily propagted segment tree. It is the data structure for the pair of a [monoid](https://en.wikipedia.org/wiki/Monoid)
-- \((S, \cdot: S \times S \to S, e \in S)\) and a set \(F\) of \(S \to S\) mappings that satisfies
-- the following properties.
--
-- - \(F\) contains the identity map \(\mathrm{id}\) such that \(\mathrm{id}(x) = x\) holds for all
-- \(x \in S\).
-- - \(F\) is closed under composition, i.e., \(f \circ g \in F\) holds for all \(f, g \in F\).
-- - \(f(x \cdot y) = f(x) \cdot f(y)\) holds for all \(f \in F\) and \(x, y \in S\).
--
-- Given an array \(S\) of length \(N\), it processes the following queries in \(O(\log N)\) time.
--
-- - Acting the map \(f\in F\) (cf. \(x = f(x)\)) on all the elements of an interval
-- - Calculating the product of the elements of an interval
--
-- In Haskell types, \(F\) is a `SegAct` (@'segAct' f@) and \(S\) is a `Monoid`. For simplicity, in
-- this document, we assume that the relevant methods work in constant time. If these they work in
-- \(O(T)\) time, each time complexity appear in this document is multipled by \(O(T)\).
--
-- ==== __Example__
-- Here we'll use `AtCoder.Extra.Monoid.Affine1` as a monoid action \(F\) and `Data.Semigroup.Sum`
-- as the acted monoid \(S\):
--
-- >>> import AtCoder.LazySegTree qualified as LST
-- >>> import AtCoder.Extra.Monoid (SegAct(..), Affine1(..)) -- `SegAct` is also re-exported in Extra.Monoid.
-- >>> import Data.Semigroup (Sum(..))
--
-- Use `build` to construct a `LazySegTree` with initial values. @'build' \@_ \@f \@a@ constructs a
-- `LazySegTree` of 'SegAct' @f a@:
--
-- >>> seg <- LST.build @_ @(Affine1 Int) @(Sum Int) $ VU.fromList [1, 2, 3, 4]
--
-- `applyIn` @seg l r f@ applies an action \(f\) to an interval \([l, r)\):
--
-- >>> LST.applyIn seg 1 3 $ Affine1 (2, 1) -- [1, 5, 7, 4]
--
-- Modify one element with `write`, `modify`, `modifyM` or `applyAt`:
--
-- >>> LST.write seg 3 $ Sum 10 -- [1, 5, 7, 10]
-- >>> LST.modify seg (+ 1) 0   -- [2, 5, 7, 10]
--
-- Read the values with `read`, `prod` or `allProd`:
--
-- >>> LST.read seg 1
-- Sum {getSum = 5}
--
-- >>> LST.prod seg 0 3 -- product (fold) of `Sum Int` in interval [0, 3)
-- Sum {getSum = 14}
--
-- >>> LST.allProd seg
-- Sum {getSum = 24}
--
-- Run binary search in \(O(\log n\) time complexity:
--
-- >>> LST.maxRight seg 0 (<= (Sum 10)) -- sum [0, 2) = 7 <= 10
-- 2
--
-- >>> LST.minLeft seg 4 (<= (Sum 10)) -- sum [3, 4) = 10 <= 10
-- 3
--
-- Inspect all the values in \(O(n \log n)\) with `freeze` or `unsafeFreeze`. Note that they
-- propagete all the applied actions:
--
-- >>> VU.map getSum <$> LST.freeze seg
-- [2,5,7,10]
--
-- ==== Tips
--
-- - `prod` returns \(a_l \cdot a_{l + 1} \cdot .. \cdot a_{r - 1}\). If you need \(a_{r - 1} \cdot a_{r - 2} \cdot .. \cdot a_{l}\),
-- wrap your monoid in `Data.Monoid.Dual`.
-- - If you ever need to store boxed types to `LazySegTree`, wrap it in 'vector:Data.Vector.Unboxed.DoNotUnboxStrict'
-- or the like.
--
-- ==== Major changes from the original @ac-library@
-- - The API is based on `Monoid` and `SegAct`, not the functions @op@, @e@, @mapping@,
-- @composition@ and @id@.
-- - @get@ and @set@ are renamed to `read` and `write`.
-- - `modify`, `modifyM`, `exchange`, `freeze` and `unsafeFreeze` are added.
--
-- @since 1.0.0.0
module AtCoder.LazySegTree
  ( -- Lazy segment tree
    SegAct (..),
    LazySegTree (nLst, sizeLst, logLst),

    -- * Constructors
    new,
    build,

    -- * Accessing elements
    write,
    modify,
    modifyM,
    exchange,
    read,

    -- * Products
    prod,
    prodMaybe,
    allProd,

    -- * Applications
    applyAt,
    applyIn,

    -- * Binary searches

    -- ** Left binary searches
    minLeft,
    minLeftM,

    -- ** Right binary searches
    maxRight,
    maxRightM,

    -- * Conversions
    freeze,
    unsafeFreeze,
  )
where

import AtCoder.Internal.Assert qualified as ACIA
import AtCoder.Internal.Bit qualified as ACIBIT
import Control.Monad (unless, when)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Bits (bit, countLeadingZeros, countTrailingZeros, testBit, (.&.), (.<<.), (.>>.))
import Data.Foldable (for_)
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Stack (HasCallStack)
import Prelude hiding (read)

-- | Typeclass reprentation of the `LazySegTree` properties. User can implement either `segAct` or
-- `segActWithLength`.
--
-- Instances should satisfy the follwing:
--
-- [Left monoid action] @'segAct' (f2 '<>' f1) x = 'segAct' f2 ('segAct' f1 x)@
-- [Identity map] @`segAct` `mempty` x = x@
-- [Endomorphism] @'segAct' f (x1 '<>' x2) = ('segAct' f x1) '<>' ('segAct' f x2)@
--
-- If you implement `segActWithLength`, satisfy one more propety:
--
-- [Linear left monoid action] @'segActWithLength' len f a = 'Data.Semigroup.stimes' len ('segAct' f a) a@.
--
-- ==== Invariant
-- In `SegAct` instances, new semigroup values are always given from the left: @new '<>' old@. The
-- order is important for non-commutative monoid implementations.
--
-- ==== __Example instance__
-- Take `AtCoder.Extra.Monoid.Affine1` as an example of type \(F\).
--
-- @
-- {-# LANGUAGE TypeFamilies #-}
--
-- import AtCoder.LazySegTree qualified as LST
-- import AtCoder.LazySegTree (SegAct (..))
-- import Data.Monoid
-- import Data.Vector.Generic qualified as VG
-- import Data.Vector.Generic.Mutable qualified as VGM
-- import Data.Vector.Unboxed qualified as VU
-- import Data.Vector.Unboxed.Mutable qualified as VUM
--
-- -- | f x = a * x + b. It's implemented as a newtype of `(a, a)` for easy `Unbox` deriving.
-- newtype 'AtCoder.Extra.Monoid.Affine1.Affine1' a = 'AtCoder.Extra.Monoid.Affine1.Affine1' ('AtCoder.Extra.Monoid.Affine1.Affine1' a)
--   deriving newtype ('Eq', 'Ord', 'Show')
--
-- -- | This type alias makes the 'Data.Vector.Unboxed.Unbox' deriving easier, described velow.
-- type 'AtCoder.Extra.Monoid.Affine1.Affine1Repr' a = (a, a)
--
-- instance ('Num' a) => 'Semigroup' ('AtCoder.Extra.Monoid.Affine1.Affine1' a) where
--   {-# INLINE ('<>') #-}
--   ('AtCoder.Extra.Monoid.Affine1.Affine1' (!a1, !b1)) '<>' ('AtCoder.Extra.Monoid.Affine1.Affine1' (!a2, !b2)) = 'AtCoder.Extra.Monoid.Affine1.Affine1' (a1 * a2, a1 * b2 + b1)
--
-- instance ('Num' a) => 'Monoid' ('AtCoder.Extra.Monoid.Affine1.Affine1' a) where
--   {-# INLINE 'mempty' #-}
--   'mempty' = 'AtCoder.Extra.Monoid.Affine1.Affine1' (1, 0)
--
-- instance ('Num' a) => 'SegAct' ('AtCoder.Extra.Monoid.Affine1.Affine1' a) ('Sum' a) where
--   {-# INLINE segActWithLength #-}
--   'segActWithLength' len ('AtCoder.Extra.Monoid.Affine1.Affine1' (!a, !b)) !x = a * x + b * fromIntegral len
-- @
--
-- Deriving 'Data.Vector.Unboxed.Unbox' is very easy for such a newtype (though the efficiency is
-- not the maximum):
--
-- @
-- newtype instance VU.MVector s ('AtCoder.Extra.Monoid.Affine1.Affine1' a) = MV_Affine1 (VU.MVector s ('AtCoder.Extra.Monoid.Affine1.Affine1' a))
-- newtype instance VU.Vector ('AtCoder.Extra.Monoid.Affine1.Affine1' a) = V_Affine1 (VU.Vector ('AtCoder.Extra.Monoid.Affine1.Affine1' a))
-- deriving instance (VU.Unbox a) => VGM.MVector VUM.MVector ('AtCoder.Extra.Monoid.Affine1.Affine1' a)
-- deriving instance (VU.Unbox a) => VG.Vector VU.Vector ('AtCoder.Extra.Monoid.Affine1.Affine1' a)
-- instance (VU.Unbox a) => VU.Unbox ('AtCoder.Extra.Monoid.Affine1.Affine1' a)
-- @
--
-- ==== __Example contest template__
-- Define your monoid action @F@ and your acted monoid @X@:
--
-- @
-- {-# LANGUAGE TypeFamilies #-}
--
-- import AtCoder.LazySegTree qualified as LST
-- import AtCoder.LazySegTree (SegAct (..))
-- import Data.Vector.Generic qualified as VG
-- import Data.Vector.Generic.Mutable qualified as VGM
-- import Data.Vector.Unboxed qualified as VU
-- import Data.Vector.Unboxed.Mutable qualified as VUM
--
-- {- ORMOLU_DISABLE -}
-- -- | `F` is a custom monoid action, defined as a newtype of `FRepr`.
-- newtype F = F FRepr deriving newtype (Eq, Ord, Show) ; unF :: F -> FRepr ; unF (F x) = x ; newtype instance VU.MVector s F = MV_F (VU.MVector s FRepr) ; newtype instance VU.Vector F = V_F (VU.Vector FRepr) ; deriving instance VGM.MVector VUM.MVector F ; deriving instance VG.Vector VU.Vector F ; instance VU.Unbox F ;
-- {- ORMOLU_ENABLE -}
--
-- -- | Affine: f x = a * x + b
-- type FRepr = (Int, Int)
--
-- instance Semigroup F where
--   -- @new <> old@
--   {-# INLINE (<>) #-}
--   (F (!a1, !b1)) <> (F (!a2, !b2)) = F (a1 * a2, a1 * b2 + b1)
--
-- instance Monoid F where
--   {-# INLINE mempty #-}
--   mempty = F (1, 0)
--
-- {- ORMOLU_DISABLE -}
-- -- | `X` is a custom acted monoid, defined as a newtype of `XRepr`.
-- newtype X = X XRepr deriving newtype (Eq, Ord, Show) ; unX :: X -> XRepr ; unX (X x) = x; newtype instance VU.MVector s X = MV_X (VU.MVector s XRepr) ; newtype instance VU.Vector X = V_X (VU.Vector XRepr) ; deriving instance VGM.MVector VUM.MVector X ; deriving instance VG.Vector VU.Vector X ; instance VU.Unbox X ;
-- {- ORMOLU_ENABLE -}
--
-- -- | Acted `Int` (same as `Sum Int`).
-- type XRepr = Int
--
-- deriving instance Num X; -- in our case `X` is a `Num`.
--
-- instance Semigroup X where
--   {-# INLINE (<>) #-}
--   (X x1) <> (X x2) = X $! x1 + x2
--
-- instance Monoid X where
--   {-# INLINE mempty #-}
--   mempty = X 0
--
-- instance SegAct F X where
--   -- {-# INLINE segAct #-}
--   -- segAct len (F (!a, !b)) (X x) = X $! a * x + b
--   {-# INLINE segActWithLength #-}
--   segActWithLength len (F (!a, !b)) (X x) = X $! a * x + len * b
-- @
--
-- It's tested as below:
--
-- @
-- expect :: (Eq a, Show a) => String -> a -> a -> ()
-- expect msg a b
--   | a == b = ()
--   | otherwise = error $ msg ++ ": expected " ++ show a ++ ", found " ++ show b
--
-- main :: IO ()
-- main = do
--   seg <- LST.build @_ @F @X $ VU.map X $ VU.fromList [1, 2, 3, 4]
--   LST.applyIn seg 1 3 $ F (2, 1) -- [1, 5, 7, 4]
--   LST.write seg 3 $ X 10 -- [1, 5, 7, 10]
--   LST.modify seg (+ (X 1)) 0   -- [2, 5, 7, 10]
--   !_ \<- (expect "test 1" (X 5)) \<$> LST.read seg 1
--   !_ \<- (expect "test 2" (X 14)) \<$> LST.prod seg 0 3 -- reads an interval [0, 3)
--   !_ \<- (expect "test 3" (X 24)) \<$> LST.allProd seg
--   !_ \<- (expect "test 4" 2) \<$> LST.maxRight seg 0 (<= (X 10)) -- sum [0, 2) = 7 <= 10
--   !_ \<- (expect "test 5" 3) \<$> LST.minLeft seg 4 (<= (X 10)) -- sum [3, 4) = 10 <= 10
--   putStrLn "=> test passed!"
-- @
--
-- @since 1.0.0.0
class (Monoid f) => SegAct f a where
  -- | Lazy segment tree action \(f(x)\).
  --
  -- @since 1.0.0.0
  {-# INLINE segAct #-}
  segAct :: f -> a -> a
  segAct = segActWithLength 1

  -- | Lazy segment tree action \(f(x)\) with the target monoid's length.
  --
  -- If you implement `SegAct` with this function, you don't have to store the monoid's length,
  -- since it's given externally.
  --
  -- @since 1.0.0.0
  {-# INLINE segActWithLength #-}
  segActWithLength :: Int -> f -> a -> a
  segActWithLength _ = segAct

-- | A lazily propagated segment tree defined around `SegAct`.
--
-- @since 1.0.0.0
data LazySegTree s f a = LazySegTree
  { -- | Valid length.
    --
    -- @since 1.0.0.0
    nLst :: {-# UNPACK #-} !Int,
    -- | \(\lceil \log_2 \mathrm{nLst} \rceil\)
    --
    -- @since 1.0.0.0
    sizeLst :: {-# UNPACK #-} !Int,
    -- | \(\log_2 \mathrm{sizeLst}\).
    --
    -- @since 1.0.0.0
    logLst :: {-# UNPACK #-} !Int,
    -- | Data storage of length @2 * sizeLst@.
    dLst :: !(VUM.MVector s a),
    -- | Data storage of length @sizeLst@.
    lzLst :: !(VUM.MVector s f)
  }

-- | Creates an array of length \(n\). All the elements are initialized to `mempty`.
--
-- ==== Constraints
-- - \(0 \leq n\)
--
-- ==== Complexity
-- - \(O(n)\)
--
-- @since 1.0.0.0
{-# INLINE new #-}
new :: (HasCallStack, PrimMonad m, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Int -> m (LazySegTree (PrimState m) f a)
new nLst
  | nLst >= 0 = build $ VU.replicate nLst mempty
  | otherwise = error $ "new: given negative size `" ++ show nLst ++ "`"

-- | Creates an array with initial values \(vs\).
--
-- ==== Constraints
-- - \(0 \leq n\)
--
-- ==== Complexity
-- - \(O(n)\)
--
-- @since 1.0.0.0
{-# INLINE build #-}
build :: (PrimMonad m, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => VU.Vector a -> m (LazySegTree (PrimState m) f a)
build vs = do
  let nLst = VU.length vs
  let sizeLst = ACIBIT.bitCeil nLst
  let logLst = countTrailingZeros sizeLst
  dLst <- VUM.replicate (2 * sizeLst) mempty
  lzLst <- VUM.replicate sizeLst mempty
  VU.iforM_ vs $ \i v -> do
    VGM.write dLst (sizeLst + i) v
  let segtree = LazySegTree {..}
  for_ [sizeLst - 1, sizeLst - 2 .. 1] $ \i -> do
    update segtree i
  pure segtree

-- | Sets \(p\)-th value of the array to \(x\).
--
-- ==== Constraints
-- - \(0 \leq p \lt n\)
--
-- ==== Complexity
-- - \(O(\log n)\)
--
-- @since 1.0.0.0
{-# INLINE write #-}
write :: (HasCallStack, PrimMonad m, SegAct f a, VU.Unbox f, Monoid a, VU.Unbox a) => LazySegTree (PrimState m) f a -> Int -> a -> m ()
write self@LazySegTree {..} p x = do
  let !_ = ACIA.checkIndex "AtCoder.LazySegTree.write" p nLst
  let p' = p + sizeLst
  for_ [logLst, logLst - 1 .. 1] $ \i -> do
    push self $ p' .>>. i
  VGM.write dLst p' x
  for_ [1 .. logLst] $ \i -> do
    update self $ p' .>>. i

-- | (Extra API) Modifies \(p\)-th value with a function \(f\).
--
-- ==== Constraints
-- - \(0 \leq p \lt n\)
--
-- ==== Complexity
-- - \(O(\log n)\)
--
-- @since 1.0.0.0
{-# INLINE modify #-}
modify :: (HasCallStack, PrimMonad m, SegAct f a, VU.Unbox f, Monoid a, VU.Unbox a) => LazySegTree (PrimState m) f a -> (a -> a) -> Int -> m ()
modify self@LazySegTree {..} f p = do
  let !_ = ACIA.checkIndex "AtCoder.LazySegTree.modify" p nLst
  let p' = p + sizeLst
  for_ [logLst, logLst - 1 .. 1] $ \i -> do
    push self $ p' .>>. i
  VGM.modify dLst f p'
  for_ [1 .. logLst] $ \i -> do
    update self $ p' .>>. i

-- | (Extra API) Modifies \(p\)-th value with a monadic function \(f\).
--
-- ==== Constraints
-- - \(0 \leq p \lt n\)
--
-- ==== Complexity
-- - \(O(\log n)\)
--
-- @since 1.0.0.0
{-# INLINE modifyM #-}
modifyM :: (HasCallStack, PrimMonad m, SegAct f a, VU.Unbox f, Monoid a, VU.Unbox a) => LazySegTree (PrimState m) f a -> (a -> m a) -> Int -> m ()
modifyM self@LazySegTree {..} f p = do
  let !_ = ACIA.checkIndex "AtCoder.LazySegTree.modify" p nLst
  let p' = p + sizeLst
  for_ [logLst, logLst - 1 .. 1] $ \i -> do
    push self $ p' .>>. i
  VGM.modifyM dLst f p'
  for_ [1 .. logLst] $ \i -> do
    update self $ p' .>>. i

-- | (Extra API) Sets \(p\)-th value of the array to \(x\) and returns the old value.
--
-- ==== Constraints
-- - \(0 \leq p \lt n\)
--
-- ==== Complexity
-- - \(O(\log n)\)
--
-- @since 1.1.0.0
{-# INLINE exchange #-}
exchange :: (HasCallStack, PrimMonad m, SegAct f a, VU.Unbox f, Monoid a, VU.Unbox a) => LazySegTree (PrimState m) f a -> Int -> a -> m a
exchange self@LazySegTree {..} p x = do
  let !_ = ACIA.checkIndex "AtCoder.LazySegTree.exchange" p nLst
  let p' = p + sizeLst
  for_ [logLst, logLst - 1 .. 1] $ \i -> do
    push self $ p' .>>. i
  res <- VGM.exchange dLst p' x
  for_ [1 .. logLst] $ \i -> do
    update self $ p' .>>. i
  pure res

-- | Returns \(p\)-th value of the array.
--
-- ==== Constraints
-- - \(0 \leq p \lt n\)
--
-- ==== Complexity
-- - \(O(\log n)\)
--
-- @since 1.0.0.0
{-# INLINE read #-}
read :: (HasCallStack, PrimMonad m, SegAct f a, VU.Unbox f, Monoid a, VU.Unbox a) => LazySegTree (PrimState m) f a -> Int -> m a
read self@LazySegTree {..} p = do
  let !_ = ACIA.checkIndex "AtCoder.LazySegTree.read" p nLst
  let p' = p + sizeLst
  for_ [logLst, logLst - 1 .. 1] $ \i -> do
    push self $ p' .>>. i
  VGM.read dLst p'

-- | Returns the product of \([a[l], ..., a[r - 1]]\), assuming the properties of the monoid. It
-- returns `mempty` if \(l = r\).
--
-- ==== Constraints
-- - \(0 \leq l \leq r \leq n\)
--
-- ==== Complexity
-- - \(O(\log n)\)
--
-- @since 1.0.0.0
{-# INLINE prod #-}
prod :: (HasCallStack, PrimMonad m, SegAct f a, VU.Unbox f, Monoid a, VU.Unbox a) => LazySegTree (PrimState m) f a -> Int -> Int -> m a
prod self@LazySegTree {nLst} l0 r0
  | not (ACIA.testInterval l0 r0 nLst) = ACIA.errorInterval "AtCoder.LazySegTree.prod" l0 r0 nLst
  | l0 == r0 = pure mempty
  | otherwise = unsafeProd self l0 r0

-- | Total variant of `prod`. Returns the product of \([a[l], ..., a[r - 1]]\), assuming the
-- properties of the monoid. It returns `Just` `mempty` if \(l = r\). It returns `Nothing` if the
-- interval is invalid.
--
-- ==== Complexity
-- - \(O(\log n)\)
--
-- @since 1.0.0.0
{-# INLINE prodMaybe #-}
prodMaybe :: (HasCallStack, PrimMonad m, SegAct f a, VU.Unbox f, Monoid a, VU.Unbox a) => LazySegTree (PrimState m) f a -> Int -> Int -> m (Maybe a)
prodMaybe self@LazySegTree {nLst} l0 r0
  | not (ACIA.testInterval l0 r0 nLst) = pure Nothing
  | l0 == r0 = pure (Just mempty)
  | otherwise = Just <$> unsafeProd self l0 r0

-- | Internal implementation of `prod`.
{-# INLINE unsafeProd #-}
unsafeProd :: (HasCallStack, PrimMonad m, SegAct f a, VU.Unbox f, Monoid a, VU.Unbox a) => LazySegTree (PrimState m) f a -> Int -> Int -> m a
unsafeProd self@LazySegTree {..} l0 r0 = do
  let l = l0 + sizeLst
  let r = r0 + sizeLst
  for_ [logLst, logLst - 1 .. 1] $ \i -> do
    when (((l .>>. i) .<<. i) /= l) $ push self $ l .>>. i
    when (((r .>>. i) .<<. i) /= r) $ push self $ (r - 1) .>>. i
  inner l (r - 1) mempty mempty
  where
    -- NOTE: we're using inclusive range [l, r] for simplicity
    inner l r !smL !smR
      | l > r = pure $! smL <> smR
      | otherwise = do
          !smL' <-
            if testBit l 0
              then (smL <>) <$> VGM.read dLst l
              else pure smL
          !smR' <-
            if not $ testBit r 0
              then (<> smR) <$> VGM.read dLst r
              else pure smR
          inner ((l + 1) .>>. 1) ((r - 1) .>>. 1) smL' smR'

-- | Returns the product of \([op(a[0], ..., a[n - 1])]\), assuming the properties of the monoid. It
-- returns `mempty` if \(n = 0\).
--
-- ==== Complexity
-- - \(O(1)\)
--
-- @since 1.0.0.0
{-# INLINE allProd #-}
allProd :: (PrimMonad m, Monoid a, VU.Unbox a) => LazySegTree (PrimState m) f a -> m a
allProd LazySegTree {..} = VGM.read dLst 1

-- | Applies @segAct f@ to an index @p@.
--
-- ==== Constraints
-- - \(0 \leq p \lt n\)
--
-- ==== Complexity
-- - \(O(\log n)\)
--
-- @since 1.0.0.0
{-# INLINE applyAt #-}
applyAt :: (HasCallStack, PrimMonad m, SegAct f a, VU.Unbox f, Monoid a, VU.Unbox a) => LazySegTree (PrimState m) f a -> Int -> f -> m ()
applyAt self@LazySegTree {..} p f = do
  let !_ = ACIA.checkIndex "AtCoder.LazySegTree.applyAt" p nLst
  let p' = p + sizeLst
  -- propagate
  for_ [logLst, logLst - 1 .. 1] $ \i -> do
    push self $ p' .>>. i
  let !len = bit $! logLst - (63 - countLeadingZeros p')
  VGM.modify dLst (segActWithLength len f) p'
  -- evaluate
  for_ [1 .. logLst] $ \i -> do
    update self $ p' .>>. i

-- | Applies @segAct f@ to an interval @[l, r)@.
--
-- ==== Constraints
-- - \(0 \leq l \leq r \leq n\)
--
-- ==== Complexity
-- - \(O(\log n)\)
--
-- @since 1.0.0.0
{-# INLINE applyIn #-}
applyIn :: (HasCallStack, PrimMonad m, SegAct f a, VU.Unbox f, Monoid a, VU.Unbox a) => LazySegTree (PrimState m) f a -> Int -> Int -> f -> m ()
applyIn self@LazySegTree {..} l0 r0 f
  | l0 == r0 = pure ()
  | otherwise = do
      let l = l0 + sizeLst
      let r = r0 + sizeLst
      -- propagate
      for_ [logLst, logLst - 1 .. 1] $ \i -> do
        when (((l .>>. i) .<<. i) /= l) $ push self (l .>>. i)
        when (((r .>>. i) .<<. i) /= r) $ push self ((r - 1) .>>. i)
      inner l (r - 1)
      -- evaluate
      for_ [1 .. logLst] $ \i -> do
        when (((l .>>. i) .<<. i) /= l) $ update self (l .>>. i)
        when (((r .>>. i) .<<. i) /= r) $ update self ((r - 1) .>>. i)
  where
    !_ = ACIA.checkInterval "AtCoder.LazySegTree.applyIn" l0 r0 nLst
    -- NOTE: we're using inclusive range [l, r] for simplicity
    inner l r
      | l > r = pure ()
      | otherwise = do
          when (testBit l 0) $ do
            allApply self l f
          unless (testBit r 0) $ do
            allApply self r f
          inner ((l + 1) .>>. 1) ((r - 1) .>>. 1)

-- | Applies a binary search on the segment tree. It returns an index \(l\) that satisfies both of the
-- following.
--
-- - \(l = r\) or \(g(a[l] \cdot a[l + 1] \cdot ... \cdot a[r - 1])\) returns `True`.
-- - \(l = 0\) or \(g(a[l - 1] \cdot a[l] \cdot ... \cdot a[r - 1])\) returns `False`.
--
-- If \(g\) is monotone, this is the minimum \(l\) that satisfies
-- \(g(a[l] \cdot a[l + 1] \cdot ... \cdot a[r - 1])\).
--
-- ==== Constraints
--
-- - if \(g\) is called with the same argument, it returns the same value, i.e., \(g\) has no side effect.
-- - @g mempty == True@.
-- - \(0 \leq r \leq n\)
--
-- ==== Complexity
-- - \(O(\log n)\)
--
-- @since 1.0.0.0
{-# INLINE minLeft #-}
minLeft :: (HasCallStack, PrimMonad m, SegAct f a, VU.Unbox f, Monoid a, VU.Unbox a) => LazySegTree (PrimState m) f a -> Int -> (a -> Bool) -> m Int
minLeft seg r0 g = minLeftM seg r0 (pure . g)

-- | Monadic variant of `minLeft`.
--
-- ==== Constraints
--
-- - if \(g\) is called with the same argument, it returns the same value, i.e., \(g\) has no side effect.
-- - @g mempty == True@.
-- - \(0 \leq r \leq n\)
--
-- ==== Complexity
-- - \(O(\log n)\)
--
-- @since 1.0.0.0
{-# INLINE minLeftM #-}
minLeftM :: (HasCallStack, PrimMonad m, SegAct f a, VU.Unbox f, Monoid a, VU.Unbox a) => LazySegTree (PrimState m) f a -> Int -> (a -> m Bool) -> m Int
minLeftM self@LazySegTree {..} r0 g = do
  b <- g mempty
  let !_ = ACIA.runtimeAssert b "AtCoder.LazySegTree.minLeftM: `g empty` returned `False`"
  if r0 == 0
    then pure 0
    else do
      let r = r0 + sizeLst
      for_ [logLst, logLst - 1 .. 1] $ \i -> do
        push self $ (r - 1) .>>. i
      inner r mempty
  where
    -- NOTE: Not ordinary bounds check!
    !_ = ACIA.runtimeAssert (0 <= r0 && r0 <= nLst) $ "AtCoder.LazySegTree.minLeftM: given invalid `right` index `" ++ show r0 ++ "` over length `" ++ show nLst ++ "`"
    inner r !sm = do
      let r' = chooseBit $ r - 1
      !sm' <- (<> sm) <$> VGM.read dLst r'
      b <- g sm'
      if not $ b
        then do
          inner2 r' sm
        else do
          if (r' .&. (-r')) /= r'
            then inner r' sm'
            else pure 0
    chooseBit r
      | r > 1 && odd r = chooseBit $ r .>>. 1
      | otherwise = r
    inner2 r sm
      | r < sizeLst = do
          push self r
          let r' = 2 * r + 1
          !sm' <- (<> sm) <$> VGM.read dLst r'
          b <- g sm'
          if b
            then inner2 (r' - 1) sm'
            else inner2 r' sm
      | otherwise = pure $ r + 1 - sizeLst

-- | Applies a binary search on the segment tree. It returns an index \(r\) that satisfies both of the
-- followings.
--
-- - \(r = l\) or \(g(a[l] \cdot a[l + 1] \cdot ... \cdot a[r - 1]))\) returns `True`.
-- - \(r = n\) or \(g(a[l] \cdot a[l + 1] \cdot ... \cdot a[r]))\) returns `False`.
--
-- If \(g\) is monotone, this is the maximum \(r\) that satisfies
-- \(g(a[l] \cdot a[l + 1] \cdot ... \cdot a[r - 1])\).
--
-- ==== Constraints
--
-- - If \(g\) is called with the same argument, it returns the same value, i.e., \(g\) has no side effect.
-- - @g mempty == True@.
-- - \(0 \leq l \leq n\)
--
-- ==== Complexity
-- - \(O(\log n)\)
--
-- @since 1.0.0.0
{-# INLINE maxRight #-}
maxRight :: (HasCallStack, PrimMonad m, SegAct f a, VU.Unbox f, Monoid a, VU.Unbox a) => LazySegTree (PrimState m) f a -> Int -> (a -> Bool) -> m Int
maxRight seg l0 g = maxRightM seg l0 (pure . g)

-- | Monadic variant of `maxRight`.
--
-- ==== Constraints
--
-- - If \(g\) is called with the same argument, it returns the same value, i.e., \(g\) has no side effect.
-- - @g mempty == True@.
-- - \(0 \leq l \leq n\)
--
-- ==== Complexity
-- - \(O(\log n)\)
--
-- @since 1.0.0.0
{-# INLINE maxRightM #-}
maxRightM :: (HasCallStack, PrimMonad m, SegAct f a, VU.Unbox f, Monoid a, VU.Unbox a) => LazySegTree (PrimState m) f a -> Int -> (a -> m Bool) -> m Int
maxRightM self@LazySegTree {..} l0 g = do
  b <- g mempty
  let !_ = ACIA.runtimeAssert b "AtCoder.LazySegTree.maxRightM: `g mempty` returned `False`"
  if l0 == nLst
    then pure nLst
    else do
      let l = l0 + sizeLst
      for_ [logLst, logLst - 1 .. 1] $ \i -> do
        push self (l .>>. i)
      inner l mempty
  where
    -- NOTE: Not ordinary bounds check!
    !_ = ACIA.runtimeAssert (0 <= l0 && l0 <= nLst) $ "AtCoder.LazySegTree.maxRightM: given invalid `left` index `" ++ show l0 ++ "` over length `" ++ show nLst ++ "`"
    inner l !sm = do
      let l' = chooseBit l
      !sm' <- (sm <>) <$> VGM.read dLst l'
      b <- g sm'
      if not $ b
        then do
          inner2 l' sm
        else do
          let l'' = l' + 1
          if (l'' .&. (-l'')) /= l''
            then inner l'' sm'
            else pure nLst
    chooseBit :: Int -> Int
    chooseBit l
      | even l = chooseBit $ l .>>. 1
      | otherwise = l
    inner2 l !sm
      | l < sizeLst = do
          push self l
          let l' = 2 * l
          !sm' <- (sm <>) <$> VGM.read dLst l'
          b <- g sm'
          if b
            then inner2 (l' + 1) sm'
            else inner2 l' sm
      | otherwise = pure $ l - sizeLst

-- | \(O(n)\) Yields an immutable copy of the mutable vector.
--
-- @since 1.0.0.0
{-# INLINE freeze #-}
freeze :: (PrimMonad m, SegAct f a, VU.Unbox f, Monoid a, VU.Unbox a) => LazySegTree (PrimState m) f a -> m (VU.Vector a)
freeze self@LazySegTree {..} = do
  -- push all (we _could_ skip some elements)
  for_ [1 .. sizeLst - 1] $ \i -> do
    push self i
  VU.freeze . VUM.take nLst $ VUM.drop sizeLst dLst

-- | \(O(n)\) Unsafely converts a mutable vector to an immutable one without copying. The mutable
-- vector may not be used after this operation.
--
-- @since 1.0.0.0
{-# INLINE unsafeFreeze #-}
unsafeFreeze :: (PrimMonad m, SegAct f a, VU.Unbox f, Monoid a, VU.Unbox a) => LazySegTree (PrimState m) f a -> m (VU.Vector a)
unsafeFreeze self@LazySegTree {..} = do
  -- push all (we _could_ skip some elements)
  for_ [1 .. sizeLst - 1] $ \i -> do
    push self i
  VU.unsafeFreeze . VUM.take nLst $ VUM.drop sizeLst dLst

-- | \(O(1)\)
{-# INLINE update #-}
update :: (HasCallStack, PrimMonad m, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => LazySegTree (PrimState m) f a -> Int -> m ()
update LazySegTree {..} k = do
  opL <- VGM.read dLst $ 2 * k
  opR <- VGM.read dLst $ 2 * k + 1
  VGM.write dLst k $! opL <> opR

-- | \(O(1)\)
{-# INLINE allApply #-}
allApply :: (HasCallStack, PrimMonad m, SegAct f a, VU.Unbox f, Monoid a, VU.Unbox a) => LazySegTree (PrimState m) f a -> Int -> f -> m ()
allApply LazySegTree {..} k f = do
  let !len = bit $! logLst - (63 - countLeadingZeros k)
  VGM.modify dLst (segActWithLength len f) k
  when (k < sizeLst) $ do
    VGM.modify lzLst (f <>) k

-- | \(O(1)\)
{-# INLINE push #-}
push :: (HasCallStack, PrimMonad m, SegAct f a, VU.Unbox f, Monoid a, VU.Unbox a) => LazySegTree (PrimState m) f a -> Int -> m ()
push self@LazySegTree {..} k = do
  lzK <- VGM.read lzLst k
  allApply self (2 * k) lzK
  allApply self (2 * k + 1) lzK
  VGM.write lzLst k mempty
