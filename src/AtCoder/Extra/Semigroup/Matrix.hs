{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}

-- | A simple HxW matrix backed by a vector, mainly for binary exponention.
--
-- The matrix is a left semigroup action: \(m_2 (m_1 v) = (m_2 \circ m_1) v\).
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
    mulToCol,
    mul,
    mulMod,
    mulMint,

    -- * Powers
    pow,
    powMod,
    powMint,

    -- * Rank
    rank,
  )
where

import AtCoder.Extra.Math qualified as ACEM
import AtCoder.Internal.Assert qualified as ACIA
import AtCoder.Internal.Barrett qualified as BT
import AtCoder.ModInt qualified as M
import Control.Monad (when)
import Control.Monad.ST (runST)
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
-- The matrix is a left semigroup action: \(m_2 (m_1 v) = (m_2 \circ m_1) v\).
--
--
-- @since 1.1.0.0
data Matrix a = Matrix
  { -- | @since 1.1.0.0
    hM :: {-# UNPACK #-} !Int,
    -- | @since 1.1.0.0
    wM :: {-# UNPACK #-} !Int,
    -- | @since 1.1.0.0
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

-- | \(O(hw)\) Creates an HxW matrix.
--
-- @since 1.1.0.0
{-# INLINE new #-}
new :: (HasCallStack, VU.Unbox a) => Int -> Int -> VU.Vector a -> Matrix a
new h w vec
  | VU.length vec /= h * w = error "AtCoder.Extra.Matrix: size mismatch"
  | otherwise = Matrix h w vec

-- | \(O(n^2)\) Creates an NxN zero matrix.
--
-- @since 1.1.0.0
{-# INLINE zero #-}
zero :: (VU.Unbox a, Num a) => Int -> Matrix a
zero n = Matrix n n $ VU.replicate (n * n) 0

-- | \(O(n^2)\) Creates an NxN identity matrix.
--
-- @since 1.1.0.0
{-# INLINE ident #-}
ident :: (VU.Unbox a, Num a) => Int -> Matrix a
ident n = Matrix n n $ VU.create $ do
  vec <- VUM.replicate (n * n) 0
  for_ [0 .. n - 1] $ \i -> do
    VGM.write vec (i + n * i) 1
  pure vec

-- FIXME: diag should not take `n`

-- | \(O(n^2)\) Creates an NxN diagonal matrix.
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

-- | \(O(h_1 K w_2)\) Multiplies H1xK matrix to a KxW2 matrix.
--
-- @since 1.1.0.0
{-# INLINE mul #-}
mul :: forall e. (Num e, VU.Unbox e) => Matrix e -> Matrix e -> Matrix e
mul (Matrix h w vecA) (Matrix h' w' vecB) = Matrix h w' $ VU.create $ do
  c <- VUM.replicate (h * w') (0 :: e)
  for_ [0 .. h - 1] $ \i -> do
    for_ [0 .. w - 1] $ \k -> do
      for_ [0 .. w' - 1] $ \j -> do
        let !aik = VG.unsafeIndex vecA (i * w + k)
        let !bkj = VG.unsafeIndex vecB (k * w' + j)
        VGM.unsafeModify c (+ (aik * bkj)) (i * w' + j)
  pure c
  where
    !_ = ACIA.runtimeAssert (w == h') "AtCoder.Extra.Matrix.mul: matrix size mismatch"

-- | \(O(h_1 w_2 K)\) Multiplies H1xK matrix to a KxW2 matrix, taking the mod.
--
-- @since 1.1.0.0
{-# INLINE mulMod #-}
mulMod :: Int -> Matrix Int -> Matrix Int -> Matrix Int
mulMod !m (Matrix h w vecA) (Matrix h' w' vecB) =
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
    -- NOTE: this is unsafe if the matrix is too large
    f row col =
      fromIntegral
        . (`rem` fromIntegral m)
        . VU.sum
        $ VU.imap
          (\iRow x -> BT.mulMod bt (fromIntegral x) (fromIntegral (VG.unsafeIndex vecB (col + (iRow * w')))))
          (VU.unsafeSlice (w * row) w vecA)
    !_ = ACIA.runtimeAssert (w == h') "AtCoder.Extra.Matrix.mulMod: matrix size mismatch"

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
mulMintImpl !bt (Matrix h w vecA) (Matrix h' w' vecB) =
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
    -- NOTE: this is unsafe if the matrix is too large
    f :: Int -> Int -> M.ModInt a
    f row col =
      M.new64
        . VU.sum
        $ VU.imap
          ( \iRow x ->
              BT.mulMod
                bt
                (fromIntegral (M.unModInt x))
                (fromIntegral (M.unModInt (VG.unsafeIndex vecB (col + (iRow * w')))))
          )
          (VU.unsafeSlice (w * row) w vecA)
    !_ = ACIA.runtimeAssert (w == h') "AtCoder.Extra.Matrix.mulMintImpl: matrix size mismatch"

-- | \(O(w n^3)\) Calculates \(M^k\).
--
-- @since 1.1.0.0
{-# INLINE pow #-}
pow :: Int -> Matrix Int -> Matrix Int
pow k mat
  | k < 0 = error "AtCoder.Extra.Matrix.pow: the exponential must be non-negative"
  | k == 0 = ident $ hM mat
  | otherwise = ACEM.power mul k mat
  where
    !_ = ACIA.runtimeAssert (hM mat == wM mat) "AtCoder.Extra.Matrix.pow: matrix size mismatch"

-- | \(O(w n^3)\) Calculates \(M^k\), taking the mod.
--
-- @since 1.1.0.0
{-# INLINE powMod #-}
powMod :: Int -> Int -> Matrix Int -> Matrix Int
powMod m k mat
  | k < 0 = error "AtCoder.Extra.Matrix.powMod: the exponential must be non-negative"
  | k == 0 = ident $ hM mat
  | otherwise = ACEM.power (mulMod m) k mat
  where
    !_ = ACIA.runtimeAssert (hM mat == wM mat) "AtCoder.Extra.Matrix.powMod: matrix size mismatch"

-- | \(O(w n^3)\) Calculates \(M^k\), specialized to `M.ModInt`.
--
-- @since 1.1.0.0
{-# INLINE powMint #-}
powMint :: forall m. (KnownNat m) => Int -> Matrix (M.ModInt m) -> Matrix (M.ModInt m)
powMint k mat
  | k < 0 = error "AtCoder.Extra.Matrix.powMint: the exponential must be non-negative"
  | k == 0 = ident $ hM mat
  | otherwise = ACEM.power (mulMintImpl bt) k mat
  where
    !_ = ACIA.runtimeAssert (hM mat == wM mat) "AtCoder.Extra.Matrix.powMint: matrix size mismatch"
    !bt = BT.new32 $ fromIntegral (natVal' (proxy# @m))

-- | (Internal)
{-# INLINE read2d #-}
read2d ::
  (PrimMonad m, VU.Unbox a) =>
  VM.MVector (PrimState m) (VUM.MVector (PrimState m) a) ->
  Int ->
  Int ->
  m a
read2d view i j = do
  row <- VGM.unsafeRead view i
  VGM.unsafeRead row j

-- | \(O(hw \min(h, w))\) Returns the rank of the matrix.
--
-- @since 1.2.0.0
{-# INLINE rank #-}
rank :: (Fractional a, Eq a, VU.Unbox a) => Matrix a -> Int
rank (Matrix h w vec) = runST $ do
  vm <- VU.thaw vec
  view <- V.thaw $ V.unfoldrExactN h (VUM.splitAt w) vm
  let inner rk j
        | rk == h || j == w = pure rk
        | otherwise = do
            xrj <- read2d view rk j
            when (xrj == 0) $ do
              let runSwap i
                    | i == h = pure ()
                    | otherwise = do
                        xij <- read2d view i j
                        if xij /= 0
                          then VGM.unsafeSwap view rk i
                          else runSwap (i + 1)
              runSwap (rk + 1)
            xrj' <- read2d view rk j
            if xrj' == 0
              then inner rk (j + 1)
              else do
                let c = 1 / xrj'
                rowRk <- VGM.read view rk
                -- for_ [j .. w - 1] $ \k -> do
                VGM.iforM_ (VGM.unsafeDrop j rowRk) $ \k_ x -> do
                  VGM.unsafeWrite rowRk (k_ + j) $! c * x
                for_ [rk + 1 .. h - 1] $ \i -> do
                  c <- read2d view i j
                  rowI <- VGM.read view i
                  -- for_ [j .. w - 1] $ \k -> do
                  VGM.iforM_ (VGM.unsafeDrop j rowRk) $ \k_ ark -> do
                    VGM.unsafeModify rowI (subtract (ark * c)) (k_ + j)
                inner (rk + 1) (j + 1)
  inner 0 0

-- | @since 1.1.0.0
instance (Num a, VU.Unbox a) => Semigroup (Matrix a) where
  {-# INLINE (<>) #-}
  (<>) = mul

  -- Prefer `powMod` or `powMint` as specialized, much efficient variant.
  {-# INLINE stimes #-}
  stimes = ACEM.power (<>) . fromIntegral
