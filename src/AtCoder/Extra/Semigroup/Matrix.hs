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
    square,
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

    -- * Inverse
    inv,
    invRaw,

    -- * Determinant
    detMod,
    detMint,
  )
where

import AtCoder.Extra.Math qualified as ACEM
import AtCoder.Internal.Assert qualified as ACIA
import AtCoder.Internal.Barrett qualified as BT
import AtCoder.ModInt qualified as M
import Control.Monad (when)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST (runST)
import Data.Foldable (for_)
import Data.Semigroup (Semigroup (..))
import Data.Vector qualified as V
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Mutable qualified as VM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import Data.Word (Word64)
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

-- | \(O(n^2)\) Creates an NxN square matrix.
--
-- @since 1.2.0.0
{-# INLINE square #-}
square :: (HasCallStack, VU.Unbox a) => Int -> VU.Vector a -> Matrix a
square n = new n n

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

-- | \(O(w n^3)\) Returns \(M^k\).
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

-- | \(O(w n^3)\) Returns \(M^k\), taking the mod.
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

-- | \(O(w n^3)\) Returns \(M^k\), specialized to `M.ModInt`.
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

-- TODO: add HasCallStack and compare their speeds

-- | \(O(n^3)\) Returns @(det, invMatrix)@ or `Nothing` if the matrix does not have inverse (the
-- determinant is zero).
--
-- ==== Constraints
-- - The input must be a square matrix.
--
-- @since 1.2.0.0
{-# INLINE inv #-}
inv :: forall a. (Fractional a, Eq a, VU.Unbox a) => Matrix a -> Maybe (a, Matrix a)
inv mat@(Matrix n _ _) = do
  (!det, !invMat) <- invRaw mat
  let !invMat' = VU.concat $ V.toList invMat
  pure (det, Matrix n n invMat')

-- | \(O(n^3)\) Returns @(det, invMatrix)@ or `Nothing` if the matrix does not have inverse (the
-- determinant is zero).
--
-- ==== Constraints
-- - The input must be a square matrix.
--
-- @since 1.2.0.0
{-# INLINE invRaw #-}
invRaw :: forall a. (Fractional a, Eq a, VU.Unbox a) => Matrix a -> Maybe (a, V.Vector (VU.Vector a))
invRaw (Matrix h w vec) = runST $ do
  vecA <- VU.thaw vec
  viewA <- V.thaw $ V.unfoldrExactN n (VUM.splitAt n) vecA

  vecB <- VUM.replicate (n * n) (0 :: a)
  for_ [0 .. n - 1] $ \i -> do
    VGM.unsafeWrite vecB (n * i + i) (1 :: a)
  viewB <- V.thaw $ V.unfoldrExactN n (VUM.splitAt n) vecB

  let inner i !det
        | i >= n = do
            viewB' <- V.mapM VU.unsafeFreeze =<< V.unsafeFreeze viewB
            pure $ Just (det, viewB')
        | otherwise = do
            let swapLoop k !det
                  | k >= n = pure det
                  | otherwise = do
                      aki <- read2d viewA k i
                      if aki /= 0
                        then do
                          if k /= i
                            then do
                              VGM.unsafeSwap viewA i k
                              VGM.unsafeSwap viewB i k
                              pure (-det)
                            else pure det
                        else do
                          swapLoop (k + 1) det
            det' <- swapLoop i det
            aii <- read2d viewA i i
            if aii == 0
              then pure Nothing
              else do
                let !c = (1 :: a) / aii
                let !det'' = det' * aii
                rowAI <- VGM.unsafeRead viewA i
                rowBI <- VGM.unsafeRead viewB i
                VUM.iforM_ (VUM.unsafeDrop i rowAI) $ \j_ x -> do
                  VGM.unsafeWrite rowAI (j_ + i) $! x * c
                VUM.iforM_ rowBI $ \j x -> do
                  VGM.unsafeWrite rowBI j $! x * c
                for_ [0 .. n - 1] $ \k -> do
                  when (i /= k) $ do
                    c <- read2d viewA k i
                    rowAK <- VGM.unsafeRead viewA k
                    rowBK <- VGM.unsafeRead viewB k
                    VGM.iforM_ (VGM.unsafeDrop i rowAI) $ \j_ aij -> do
                      VGM.unsafeModify rowAK (subtract (aij * c)) (j_ + i)
                    VGM.iforM_ rowBI $ \j bij -> do
                      VGM.unsafeModify rowBK (subtract (bij * c)) j
                inner (i + 1) det''

  inner 0 (1 :: a)
  where
    !_ = ACIA.runtimeAssert (h == w) $ "AtCoder.Extra.Semigroup.Matrix.inv: given non-square matrix of size " ++ show (h, w)
    n = h

-- | \(O(hw \min(h, w))\) Returns the rank of the matrix.
--
-- @since 1.2.0.0
{-# INLINE detMod #-}
detMod :: Int -> Matrix Int -> Int
detMod m (Matrix h w vecA) = runST $ do
  vm <- VU.thaw vecA
  view <- V.thaw $ V.unfoldrExactN n (VUM.splitAt n) vm

  let inner i (!det :: Int)
        | i >= n = pure det
        | otherwise = do
            let swapLoop j !det
                  | j >= n = pure det
                  | otherwise = do
                      aji <- read2d view j i
                      if aji == 0
                        then swapLoop (j + 1) det
                        else do
                          if i /= j
                            then do
                              VGM.unsafeSwap view i j
                              pure $! m - det
                            else pure det
            det' <- swapLoop i det
            det'' <- VU.foldM'
              ( \ !acc j -> do
                  let visitDiag !det = do
                        aii <- read2d view i i
                        if aii == 0
                          then pure det
                          else do
                            aji <- read2d view j i
                            let !c = m - aji `div` aii
                            rowI <- VGM.unsafeRead view i
                            rowJ <- VGM.unsafeRead view j
                            -- NOTE: it's a reverse loop!
                            VGM.ifoldrM'
                              ( \k_ aik () -> do
                                  VGM.unsafeModify rowJ ((`mod` m) . (+ aik * c)) (k_ + i)
                              )
                              ()
                              (VGM.unsafeDrop i rowI)
                            VGM.unsafeSwap view i j
                            visitDiag (m - det)
                  acc' <- visitDiag acc
                  VGM.unsafeSwap view i j
                  pure $! m - acc'
              )
              det'
              (VU.generate (n - (i + 1)) (+ (i + 1)))

            inner (i + 1) det''

  det <- inner 0 (1 :: Int)
  fromIntegral
    <$> VU.foldM'
      ( \(!acc :: Word64) i -> do
          aii <- read2d view i i
          pure $! BT.mulMod bt acc $! fromIntegral aii
      )
      (fromIntegral det)
      (VU.generate n id)
  where
    !_ = ACIA.runtimeAssert (h == w) $ "AtCoder.Extra.Semigroup.Matrix.detMod: given non-square matrix of size " ++ show (h, w)
    !n = h
    !bt = BT.new32 $ fromIntegral m

-- | \(O(hw \min(h, w))\) Returns the rank of the matrix.
--
-- @since 1.2.0.0
{-# INLINE detMint #-}
detMint :: forall a. (KnownNat a) => Matrix (M.ModInt a) -> M.ModInt a
detMint matA = M.new . detMod m $ map M.val matA
  where
    !m = fromIntegral (natVal' (proxy# @a))

-- | @since 1.1.0.0
instance (Num a, VU.Unbox a) => Semigroup (Matrix a) where
  {-# INLINE (<>) #-}
  (<>) = mul

  -- Prefer `powMod` or `powMint` as specialized, much efficient variant.
  {-# INLINE stimes #-}
  stimes = ACEM.power (<>) . fromIntegral
