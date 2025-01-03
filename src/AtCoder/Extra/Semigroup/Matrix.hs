{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}

-- | A simple HxW matrix backed by a vector, mainly for binary exponention.
--
-- The matrix is considered to be left semigroup action: \(m_2 (m_1 v) = (m_2 \circ m_1) v\).
--
-- @since 1.1.0.0
module AtCoder.Extra.Semigroup.Matrix
  ( -- * Matrix
    Matrix (..),

    -- * Constructors
    new,
    zero,
    ident,
    diag,

    -- * Mapping
    map,

    -- * Multiplications

    -- ** To a column vector
    mulToCol,

    -- ** To another matrix
    mul,
    mulMint,
    mulMod,
    powMod,
    powMint,
  )
where

import AtCoder.Extra.Math qualified as ACEM
import AtCoder.Internal.Assert qualified as ACIA
import AtCoder.Internal.Barrett qualified as BT
import AtCoder.ModInt qualified as M
import Data.Foldable (for_)
import Data.Semigroup (Semigroup (..))
import Data.Vector qualified as V
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Exts (proxy#)
import GHC.Stack (HasCallStack)
import GHC.TypeNats (KnownNat, natVal')
import Prelude hiding (map)

-- | A simple HxW matrix backed by a vector, mainly for binary exponention.
--
-- The matrix is considered to be left semigroup action: \(m_2 (m_1 v) = (m_2 \circ m_1) v\).
--
--
-- @since 1.1.0.0
data Matrix a = Matrix
  { hM :: {-# UNPACK #-} !Int,
    wM :: {-# UNPACK #-} !Int,
    vecM :: !(VU.Vector a)
  }
  deriving
    ( -- | @since 1.1.0.0
      Show,
      -- | @since 1.1.0.0
      Eq
    )

-- | Type alias of a column vector.
--
-- @since 1.1.0.0
type Col a = VU.Vector a

-- | \(O(hw)\) Creates a HxW matrix.
--
-- @since 1.1.0.0
{-# INLINE new #-}
new :: (HasCallStack, VU.Unbox a) => Int -> Int -> VU.Vector a -> Matrix a
new h w vec
  | VU.length vec /= h * w = error "AtCoder.Extra.Matrix: size mismatch"
  | otherwise = Matrix h w vec

-- | \(O(n^2)\) Creates a NxN zero matrix.
--
-- @since 1.1.0.0
{-# INLINE zero #-}
zero :: (VU.Unbox a, Num a) => Int -> Matrix a
zero n = Matrix n n $ VU.replicate (n * n) 0

-- | \(O(n^2)\) Creates a NxN identity matrix.
--
-- @since 1.1.0.0
{-# INLINE ident #-}
ident :: (VU.Unbox a, Num a) => Int -> Matrix a
ident n = Matrix n n $ VU.create $ do
  vec <- VUM.replicate (n * n) 0
  for_ [0 .. n - 1] $ \i -> do
    VGM.write vec (i + n * i) 1
  pure vec

-- | \(O(n^2)\) Creates a NxN diagonal matrix.
--
-- @since 1.1.0.0
{-# INLINE diag #-}
diag :: (VU.Unbox a, Num a) => Int -> VU.Vector a -> Matrix a
diag n xs = Matrix n n $ VU.create $ do
  vec <- VUM.replicate (n * n) 0
  VU.iforM_ xs $ \i x -> do
    VGM.write vec (i + n * i) x
  pure vec

-- | \(O(n^2)\) Maps the `Matrix`.
--
-- @since 1.1.0.0
{-# INLINE map #-}
map :: (VU.Unbox a, VU.Unbox b) => (a -> b) -> Matrix a -> Matrix b
map f Matrix {..} = Matrix hM wM $ VU.map f vecM

-- | \(O(hw)\) Multiplies HxW matrix to a Hx1 column vector.
--
-- @since 1.1.0.0
{-# INLINE mulToCol #-}
mulToCol :: (Num a, VU.Unbox a) => Matrix a -> Col a -> Col a
mulToCol Matrix {..} !col = VU.convert $ V.map (VU.sum . VU.zipWith (*) col) rows
  where
    !n = VU.length col
    !_ = ACIA.runtimeAssert (n == wM) "AtCoder.Extra.Matrix.mulToCol: size mismatch"
    rows = V.unfoldrExactN hM (VU.splitAt wM) vecM

-- | \(O(h_1 w_2 K)\) Multiplies H1xK matrix to a KxW2 matrix.
--
-- @since 1.1.0.0
{-# INLINE mul #-}
mul :: (Num e, VU.Unbox e) => Matrix e -> Matrix e -> Matrix e
mul !a !b =
  Matrix h w' $
    VU.unfoldrExactN
      (h * w')
      ( \(!row, !col) ->
          let !x = f row col
           in if col + 1 >= w'
                then (x, (row + 1, 0))
                else (x, (row, col + 1))
      )
      (0, 0)
  where
    f row col = VU.sum $ VU.imap (\iRow x -> x * VG.unsafeIndex vecB (col + iRow * w')) (VU.unsafeSlice (w * row) w vecA)
    h = hM a
    w = wM a
    h' = hM b
    vecA = vecM a
    w' = wM b
    vecB = vecM b
    !_ = ACIA.runtimeAssert (w == h') "AtCoder.Extra.Matrix.mul: matrix size mismatch"

-- | \(O(h_1 w_2 K)\) `mul` specialized to `M.ModInt`.
--
-- @since 1.1.0.0
{-# INLINE mulMint #-}
mulMint :: forall a. (KnownNat a) => Matrix (M.ModInt a) -> Matrix (M.ModInt a) -> Matrix (M.ModInt a)
mulMint = mulMintImpl bt
  where
    !bt = BT.new32 $ fromIntegral (natVal' (proxy# @a))

{-# INLINE mulMintImpl #-}
mulMintImpl :: forall a. (KnownNat a) => BT.Barrett -> Matrix (M.ModInt a) -> Matrix (M.ModInt a) -> Matrix (M.ModInt a)
mulMintImpl !bt !a !b =
  Matrix h w' $
    VU.unfoldrExactN
      (h * w')
      ( \(!row, !col) ->
          let !x = f row col
           in if col + 1 >= w'
                then (x, (row + 1, 0))
                else (x, (row, col + 1))
      )
      (0, 0)
  where
    f :: Int -> Int -> M.ModInt a
    f row col = VU.sum $ VU.imap (\iRow x -> mulMod_ x (VG.unsafeIndex vecB (col + (iRow * w')))) (VU.unsafeSlice (w * row) w vecA)
    mulMod_ :: M.ModInt a -> M.ModInt a -> M.ModInt a
    mulMod_ (M.ModInt x) (M.ModInt y) = M.unsafeNew . fromIntegral $ BT.mulMod bt (fromIntegral x) (fromIntegral y)
    h = hM a
    w = wM a
    h' = hM b
    vecA = vecM a
    w' = wM b
    vecB = vecM b
    !_ = ACIA.runtimeAssert (w == h') "AtCoder.Extra.Matrix.mulMint: matrix size mismatch"

-- | \(O(h_1 w_2 K)\) Multiplies H1xK matrix to a KxW2 matrix, taking the modulus value.
--
-- @since 1.1.0.0
{-# INLINE mulMod #-}
mulMod :: Int -> Matrix Int -> Matrix Int -> Matrix Int
mulMod !m !a !b =
  Matrix h w' $
    VU.unfoldrExactN
      (h * w')
      ( \(!row, !col) ->
          let !x = f row col
           in if col + 1 >= w'
                then (x, (row + 1, 0))
                else (x, (row, col + 1))
      )
      (0, 0)
  where
    !bt = BT.new32 $ fromIntegral m
    f row col = VU.foldl1' addMod $ VU.imap (\iRow x -> mulMod_ x (VG.unsafeIndex vecB (col + (iRow * w')))) (VU.unsafeSlice (w * row) w vecA)
    addMod x y = (x + y) `rem` m
    mulMod_ x y = fromIntegral $ BT.mulMod bt (fromIntegral x) (fromIntegral y)
    h = hM a
    w = wM a
    h' = hM b
    vecA = vecM a
    w' = wM b
    vecB = vecM b
    !_ = ACIA.runtimeAssert (w == h') "AtCoder.Extra.Matrix.mulMod: matrix size mismatch"

-- | \(O(nhw)\) Calculates \(M^n\).
{-# INLINE powMod #-}
powMod :: Int -> Int -> Matrix Int -> Matrix Int
powMod m n mat
  | n == 0 = ident $ hM mat
  | otherwise = ACEM.power (mulMod m) n mat
  where
    !_ = ACIA.runtimeAssert (hM mat == wM mat) "AtCoder.Extra.Matrix.powMod: matrix size mismatch"

-- | \(O(nhw)\) Calculates \(M^n\), specialized to `M.ModInt`.
powMint :: forall m. (KnownNat m) => Int -> Matrix (M.ModInt m) -> Matrix (M.ModInt m)
powMint n mat
  | n == 0 = ident $ hM mat
  | otherwise = ACEM.power (mulMintImpl bt) n mat
  where
    !_ = ACIA.runtimeAssert (hM mat == wM mat) "AtCoder.Extra.Matrix.powMint: matrix size mismatch"
    !bt = BT.new32 $ fromIntegral (natVal' (proxy# @m))

-- | @since 1.1.0.0
instance (Num a, VU.Unbox a) => Semigroup (Matrix a) where
  {-# INLINE (<>) #-}
  (<>) = mul
  {-# INLINE stimes #-}
  stimes = ACEM.power (<>) . fromIntegral
