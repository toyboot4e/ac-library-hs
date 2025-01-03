{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}

module BenchLib.Matrix
  ( Matrix (..),
    new,
    map,
    mulToCol,
    mulToColModInt,
    mul1,
    mul2,
    mul3_1,
    mul3_2,
    mul3_3,
    mulMod1,
    mulMod2,
    mulMod3,
    mulMod4,
    mulMod5,
    mulMint1,
    mulMint2,
    mulMint3,
  )
where

import AtCoder.Internal.Assert qualified as ACIA
import AtCoder.Internal.Barrett qualified as BT
import AtCoder.ModInt qualified as M
import Data.Vector qualified as V
import Data.Vector.Generic qualified as VG
import Data.Vector.Unboxed qualified as VU
import Data.Word (Word64)
import GHC.Exts (Proxy#, proxy#)
import GHC.Stack (HasCallStack)
import GHC.TypeNats (KnownNat, natVal, natVal')
import Prelude hiding (map)

data Matrix a = Matrix
  { hM :: {-# UNPACK #-} !Int,
    wM :: {-# UNPACK #-} !Int,
    vecM :: !(VU.Vector a)
  }
  deriving (Show, Eq)

type Col a = VU.Vector a

{-# INLINE new #-}
new :: (HasCallStack, VU.Unbox a) => Int -> Int -> VU.Vector a -> Matrix a
new h w vec
  | VU.length vec /= h * w = error "AtCoder.Extra.Matrix: size mismatch"
  | otherwise = Matrix h w vec

{-# INLINE map #-}
map :: (VU.Unbox a, VU.Unbox b) => (a -> b) -> Matrix a -> Matrix b
map f Matrix {..} = Matrix hM wM $ VU.map f vecM

{-# INLINE mulToCol #-}
mulToCol :: (Num a, VU.Unbox a) => Matrix a -> Col a -> Col a
mulToCol Matrix {..} !col = VU.convert $ V.map (VU.sum . VU.zipWith (*) col) rows
  where
    !n = VU.length col
    !_ = ACIA.runtimeAssert (n == wM) "AtCoder.Extra.Matrix.mulToCol: size mismatch"
    rows = V.unfoldrExactN hM (VU.splitAt wM) vecM

{-# INLINE mulToColModInt #-}
mulToColModInt :: forall m. (KnownNat m) => Matrix (M.ModInt m) -> Col (M.ModInt m) -> Col (M.ModInt m)
mulToColModInt Matrix {..} !col = VU.convert $ V.map (VU.foldl' (+) (M.unsafeNew 0) . VU.zipWith mulMod col) rows
  where
    !_ = ACIA.runtimeAssert (VU.length col == wM) "AtCoder.Extra.Matrix.mulToColModInt: size mismatch"
    !bt = BT.new32 $ fromIntegral (natVal' (proxy# @m))
    rows = V.unfoldrExactN hM (VU.splitAt wM) vecM
    mulMod (M.ModInt x) (M.ModInt y) = M.unsafeNew . fromIntegral $ BT.mulMod bt (fromIntegral x) (fromIntegral y)

{-# INLINE mul1 #-}
mul1 :: (Num e, VU.Unbox e) => Matrix e -> Matrix e -> Matrix e
mul1 !a !b =
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
    f row col = VU.sum $ VU.zipWith (*) (rows1 VG.! row) (cols2 VG.! col)
    h = hM a
    w = wM a
    vecA = vecM a
    h' = hM b
    w' = wM b
    vecB = vecM b
    !_ = ACIA.runtimeAssert (w == h') "AtCoder.Extra.Matrix.mul: matrix size mismatch"
    rows1 = V.unfoldrExactN h (VU.splitAt w) vecA
    cols2 = V.generate w' $ \col -> VU.unfoldrExactN h' (\i -> (VG.unsafeIndex vecB i, i + w')) col

{-# INLINE mul2 #-}
mul2 :: (Num e, VU.Unbox e) => Matrix e -> Matrix e -> Matrix e
mul2 !a !b =
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
    f row col = VU.sum $ VU.imap (\iRow x -> x * VG.unsafeIndex vecB (col + iRow * w')) (rows1 VG.! row)
    h = hM a
    w = wM a
    vecA = vecM a
    h' = hM b
    w' = wM b
    vecB = vecM b
    !_ = ACIA.runtimeAssert (w == h') "AtCoder.Extra.Matrix.mul: matrix size mismatch"
    rows1 = V.unfoldrExactN h (VU.splitAt w) vecA

{-# INLINE mul3_1 #-}
mul3_1 :: (Num e, VU.Unbox e) => Matrix e -> Matrix e -> Matrix e
mul3_1 !a !b =
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

{-# INLINE mul3_2 #-}
mul3_2 :: (Num e, VU.Unbox e) => Matrix e -> Matrix e -> Matrix e
mul3_2 !a !b =
  Matrix h w' $
    VU.unfoldrExactN
      (h * w')
      ( \(!row, !col) ->
          let !y = VU.sum $ VU.imap (\iRow x -> x * VG.unsafeIndex vecB (col + iRow * w')) (VU.unsafeSlice (w * row) w vecA)
           in if col + 1 >= w'
                then (y, (row + 1, 0))
                else (y, (row, col + 1))
      )
      (0, 0)
  where
    h = hM a
    w = wM a
    h' = hM b
    vecA = vecM a
    w' = wM b
    vecB = vecM b
    !_ = ACIA.runtimeAssert (w == h') "AtCoder.Extra.Matrix.mul: matrix size mismatch"

{-# INLINE mul3_3 #-}
mul3_3 :: (Num e, VU.Unbox e) => Matrix e -> Matrix e -> Matrix e
mul3_3 !a !b = Matrix h w' $ VU.generate (h * w') $ \i ->
  let (!row, !col) = i `quotRem` w'
   in VU.sum $ VU.imap (\iRow x -> x * VG.unsafeIndex vecB (col + iRow * w')) (VU.unsafeSlice (w * row) w vecA)
  where
    h = hM a
    w = wM a
    h' = hM b
    vecA = vecM a
    w' = wM b
    vecB = vecM b
    !_ = ACIA.runtimeAssert (w == h') "AtCoder.Extra.Matrix.mul: matrix size mismatch"

{-# INLINE mulMod1 #-}
mulMod1 :: Int -> Matrix Int -> Matrix Int -> Matrix Int
mulMod1 !m !a !b =
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
    f row col = VU.foldl1' addMod $ VU.imap (\iRow x -> mulMod x (VG.unsafeIndex vecB (col + (iRow * w')))) (VU.unsafeSlice (w * row) w vecA)
    addMod x y = (x + y) `mod` m
    mulMod x y = (x * y) `mod` m
    h = hM a
    w = wM a
    h' = hM b
    vecA = vecM a
    w' = wM b
    vecB = vecM b
    !_ = ACIA.runtimeAssert (w == h') "AtCoder.Extra.Matrix.mul: matrix size mismatch"

{-# INLINE mulMod2 #-}
mulMod2 :: Int -> Matrix Int -> Matrix Int -> Matrix Int
mulMod2 !m !a !b =
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
    f row col = VU.foldl1' addMod $ VU.imap (\iRow x -> mulMod x (VG.unsafeIndex vecB (col + (iRow * w')))) (VU.unsafeSlice (w * row) w vecA)
    -- very slow
    addMod x y
      | x + y >= m = x + y - m
      | otherwise = x + y
    mulMod x y = (x * y) `mod` m
    h = hM a
    w = wM a
    h' = hM b
    vecA = vecM a
    w' = wM b
    vecB = vecM b
    !_ = ACIA.runtimeAssert (w == h') "AtCoder.Extra.Matrix.mul: matrix size mismatch"

{-# INLINE mulMod3 #-}
mulMod3 :: Int -> Matrix Int -> Matrix Int -> Matrix Int
mulMod3 !m !a !b =
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
    f row col = VU.foldl1' addMod $ VU.imap (\iRow x -> mulMod x (VG.unsafeIndex vecB (col + (iRow * w')))) (VU.unsafeSlice (w * row) w vecA)
    addMod x y = (x + y) `mod` m
    mulMod x y = fromIntegral $ BT.mulMod bt (fromIntegral x) (fromIntegral y)
    h = hM a
    w = wM a
    h' = hM b
    vecA = vecM a
    w' = wM b
    vecB = vecM b
    !_ = ACIA.runtimeAssert (w == h') "AtCoder.Extra.Matrix.mul: matrix size mismatch"

{-# INLINE mulMod4 #-}
mulMod4 :: Int -> Matrix Int -> Matrix Int -> Matrix Int
mulMod4 !m !a !b =
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
    f row col = VU.foldl1' addMod $ VU.imap (\iRow x -> mulMod x (VG.unsafeIndex vecB (col + (iRow * w')))) (VU.unsafeSlice (w * row) w vecA)
    addMod x y = (x + y) `rem` m
    mulMod x y = fromIntegral $ BT.mulMod bt (fromIntegral x) (fromIntegral y)
    h = hM a
    w = wM a
    h' = hM b
    vecA = vecM a
    w' = wM b
    vecB = vecM b
    !_ = ACIA.runtimeAssert (w == h') "AtCoder.Extra.Matrix.mul: matrix size mismatch"

{-# INLINE mulMod5 #-}
mulMod5 :: Int -> Matrix Int -> Matrix Int -> Matrix Int
mulMod5 !m !a !b =
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
    f row col = VU.foldl1' addMod $ VU.imap (\iRow x -> mulMod x (VG.unsafeIndex vecB (col + (iRow * w')))) (VU.unsafeSlice (w * row) w vecA)
    addMod x y
      | x + y >= m = x + y - m
      | otherwise = x + y
    mulMod x y = fromIntegral $ BT.mulMod bt (fromIntegral x) (fromIntegral y)
    h = hM a
    w = wM a
    h' = hM b
    vecA = vecM a
    w' = wM b
    vecB = vecM b
    !_ = ACIA.runtimeAssert (w == h') "AtCoder.Extra.Matrix.mul: matrix size mismatch"

{-# INLINE mulMint1 #-}
mulMint1 :: forall a. (KnownNat a) => Matrix (M.ModInt a) -> Matrix (M.ModInt a) -> Matrix (M.ModInt a)
mulMint1 !a !b =
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
    f row col = VU.sum $ VU.imap (\iRow x -> mulMod x (VG.unsafeIndex vecB (col + (iRow * w')))) (VU.unsafeSlice (w * row) w vecA)
    mulMod :: M.ModInt a -> M.ModInt a -> M.ModInt a
    mulMod = (*)
    h = hM a
    w = wM a
    h' = hM b
    vecA = vecM a
    w' = wM b
    vecB = vecM b
    !_ = ACIA.runtimeAssert (w == h') "AtCoder.Extra.Matrix.mul: matrix size mismatch"

{-# INLINE mulMint2 #-}
mulMint2 :: forall a. (KnownNat a) => Matrix (M.ModInt a) -> Matrix (M.ModInt a) -> Matrix (M.ModInt a)
mulMint2 !a !b =
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
    !bt = BT.new32 $ fromIntegral (natVal' (proxy# @a))
    f :: Int -> Int -> M.ModInt a
    f row col = VU.sum $ VU.imap (\iRow x -> mulMod x (VG.unsafeIndex vecB (col + (iRow * w')))) (VU.unsafeSlice (w * row) w vecA)
    mulMod :: M.ModInt a -> M.ModInt a -> M.ModInt a
    mulMod (M.ModInt x) (M.ModInt y) = M.unsafeNew . fromIntegral $ BT.mulMod bt (fromIntegral x) (fromIntegral y)
    h = hM a
    w = wM a
    h' = hM b
    vecA = vecM a
    w' = wM b
    vecB = vecM b
    !_ = ACIA.runtimeAssert (w == h') "AtCoder.Extra.Matrix.mul: matrix size mismatch"

-- REMARK: This is very unsafe in that it can overflow (mod^2 * n)
{-# INLINE mulMint3 #-}
mulMint3 :: forall a. (KnownNat a) => Matrix (M.ModInt a) -> Matrix (M.ModInt a) -> Matrix (M.ModInt a)
mulMint3 !a !b =
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
    !bt = BT.new32 $ fromIntegral (natVal' (proxy# @a))
    f :: Int -> Int -> M.ModInt a
    f row col = M.new64 . VU.sum $ VU.imap (\iRow x -> mulMod x (VG.unsafeIndex vecB (col + (iRow * w')))) (VU.unsafeSlice (w * row) w vecA)
    mulMod :: M.ModInt a -> M.ModInt a -> Word64
    mulMod (M.ModInt x) (M.ModInt y) = BT.mulMod bt (fromIntegral x) (fromIntegral y)
    h = hM a
    w = wM a
    h' = hM b
    vecA = vecM a
    w' = wM b
    vecB = vecM b
    !_ = ACIA.runtimeAssert (w == h') "AtCoder.Extra.Matrix.mul: matrix size mismatch"
