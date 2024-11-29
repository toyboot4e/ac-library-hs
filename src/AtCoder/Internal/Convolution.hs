{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}

-- | It calculates \((+,\times)\) convolution. Given two arrays \(a_0, a_1, \cdots, a_{N - 1}\) and
-- \(b_0, b_1, \cdots, b_{M - 1}\), it calculates the array \(c\) of length \(N + M - 1\), defined
-- by
--
-- \[
-- c_i = \sum_{j = 0}^i a_j b_{i - j}
-- \]
module AtCoder.Internal.Convolution
  ( FftInfo,
    newInfo,
    butterfly,
    butterflyInv,
    convolutionNaive,
    convolutionFft,
  )
where

import AtCoder.Internal.Bit qualified as ACIB
import AtCoder.ModInt qualified as AM
import Control.Monad (when)
import Control.Monad.Fix (fix)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Bits (bit, complement, countTrailingZeros, (.<<.), (.>>.))
import Data.Foldable
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Exts (proxy#)
import GHC.TypeLits (natVal')

data FftInfo p = FftInfo
  { rootFft :: !(VU.Vector (AM.StaticModInt p)),
    iRootFft :: !(VU.Vector (AM.StaticModInt p)),
    rate2Fft :: !(VU.Vector (AM.StaticModInt p)),
    iRate2Fft :: !(VU.Vector (AM.StaticModInt p)),
    rate3Fft :: !(VU.Vector (AM.StaticModInt p)),
    iRate3Fft :: !(VU.Vector (AM.StaticModInt p))
  }

-- | TODO: should be evaluated at compile time?
newInfo :: forall m p. (PrimMonad m, AM.Modulus p) => m (FftInfo p)
newInfo = do
  let !g = AM.primitiveRootModulus (proxy# @p)
  let !m = fromInteger $ natVal' (proxy# @p)
  let !rank2 = countTrailingZeros $ m - 1

  root <- VUM.unsafeNew (rank2 + 1)
  iRoot <- VUM.unsafeNew (rank2 + 1)
  rate2 <- VUM.unsafeNew (max 0 (rank2 - 2 + 1))
  iRate2 <- VUM.unsafeNew (max 0 (rank2 - 2 + 1))
  rate3 <- VUM.unsafeNew (max 0 (rank2 - 3 + 1))
  iRate3 <- VUM.unsafeNew (max 0 (rank2 - 3 + 1))

  VGM.write root rank2 . AM.pow (AM.new g) $ (m - 1) .>>. rank2
  VGM.write iRoot rank2 . AM.inv =<< VGM.read root rank2
  for_ [rank2 - 1, rank2 - 2 .. 0] $ \i -> do
    r <- VGM.read root (i + 1)
    ir <- VGM.read iRoot (i + 1)
    VGM.write root i $! r * r
    VGM.write iRoot i $! ir * ir

  VU.foldM'_
    ( \(!prod, !iProd) i -> do
        r <- VGM.read root (i + 2)
        ir <- VGM.read iRoot (i + 2)
        VGM.write rate2 i $! r * prod
        VGM.write iRate2 i $! ir * iProd
        pure (prod * ir, iProd * r)
    )
    (1, 1)
    (VU.generate (rank2 - 1) id)

  VU.foldM'_
    ( \(!prod, !iProd) i -> do
        r <- VGM.read root (i + 3)
        ir <- VGM.read iRoot (i + 3)
        VGM.write rate3 i $! r * prod
        VGM.write iRate3 i $! ir * iProd
        pure (prod * ir, iProd * r)
    )
    (1, 1)
    (VU.generate (rank2 - 2) id)

  rootFft <- VU.unsafeFreeze root
  iRootFft <- VU.unsafeFreeze iRoot
  rate2Fft <- VU.unsafeFreeze rate2
  iRate2Fft <- VU.unsafeFreeze iRate2
  rate3Fft <- VU.unsafeFreeze rate3
  iRate3Fft <- VU.unsafeFreeze iRate3
  pure FftInfo {..}

butterfly ::
  forall m p.
  (PrimMonad m, AM.Modulus p) =>
  VUM.MVector (PrimState m) (AM.StaticModInt p) ->
  m ()
butterfly a = do
  let n = VUM.length a
  let h = countTrailingZeros n
  let !m = fromInteger $ natVal' (proxy# @p)

  FftInfo {..} <- newInfo @_ @p
  flip fix 0 $ \loop len -> do
    when (len < h) $ do
      if h - len == 1
        then do
          let !p = bit $ h - len - 1
          VU.foldM'_
            ( \ !rot s -> do
                let offset = s .<<. (h - len)
                for_ [0 .. p - 1] $ \i -> do
                  l <- VGM.read a $ i + offset
                  r <- (* rot) <$> VGM.read a (i + offset + p)
                  VGM.write a (i + offset) $! l + r
                  VGM.write a (i + offset + p) $! l - r
                if s + 1 /= bit len
                  then pure . (rot *) $ rate2Fft VG.! countTrailingZeros (complement s)
                  else pure rot
            )
            (AM.new32 @p 1)
            (VU.generate (bit len) id)
          loop $ len + 1 -- break
        else do
          -- 4-base
          let p = bit $ h - len - 2
          let !imag = AM.val $ rootFft VG.! 2
          VU.foldM'_
            ( \ !rot s -> do
                let !rot1 :: Int = AM.val rot
                let !rot2_ = rot * rot
                let !rot2 :: Int = AM.val rot2_
                let !rot3 :: Int = AM.val $ rot2_ * rot
                let !offset = s .<<. (h - len)
                let !mod2 = m * m
                for_ [0 .. p - 1] $ \i -> do
                  !a0 :: Int <- AM.val <$> VGM.read a (i + offset)
                  !a1 :: Int <- (* rot1) . AM.val <$> VGM.read a (i + offset + p)
                  !a2 :: Int <- (* rot2) . AM.val <$> VGM.read a (i + offset + 2 * p)
                  !a3 :: Int <- (* rot3) . AM.val <$> VGM.read a (i + offset + 3 * p)
                  let !a1na3imag = (a1 + mod2 - a3) `mod` m * imag
                  let !na2 = mod2 - a2
                  VGM.write a (i + offset) . AM.new $! a0 + a2 + a1 + a3
                  VGM.write a (i + offset + 1 * p) . AM.new $! a0 + a2 + (2 * mod2 - (a1 + a3))
                  VGM.write a (i + offset + 2 * p) . AM.new $! a0 + na2 + a1na3imag
                  VGM.write a (i + offset + 3 * p) . AM.new $! a0 + na2 + (mod2 - a1na3imag)
                if s + 1 /= bit len
                  then pure . (rot *) $ rate3Fft VG.! countTrailingZeros (complement s)
                  else pure rot
            )
            (AM.raw32 @p 1)
            (VU.generate (bit len) id)
          loop $ len + 2

butterflyInv ::
  forall m p.
  (PrimMonad m, AM.Modulus p) =>
  VUM.MVector (PrimState m) (AM.StaticModInt p) ->
  m ()
butterflyInv a = do
  let n = VUM.length a
  let h = countTrailingZeros n
  let !m = fromInteger $ natVal' (proxy# @p)

  FftInfo {..} <- newInfo @_ @p
  flip fix h $ \loop len -> do
    when (len /= 0) $ do
      if len == 1
        then do
          let p = bit $ h - len
          VU.foldM'_
            ( \ !irot s -> do
                let !offset = s .<<. (h - len + 1)
                for_ [0 .. p - 1] $ \i -> do
                  l <- VGM.read a $ i + offset
                  r <- VGM.read a $ i + offset + p
                  VGM.write a (i + offset) $! l + r
                  VGM.write a (i + offset + p) . AM.new $! (m + AM.val l - AM.val r) * AM.val irot
                if s + 1 /= bit (len - 1)
                  then pure . (irot *) $ iRate2Fft VG.! countTrailingZeros (complement s)
                  else pure irot
            )
            (AM.new32 @p 1)
            (VU.generate (bit (len - 1)) id)
          loop $ len - 1
        else do
          -- 4-base
          let p = bit $ h - len
          let !iimag = AM.val $ iRootFft VG.! 2
          VU.foldM'_
            ( \ !irot s -> do
                let !irot1 :: Int = AM.val irot
                let !irot2_ = irot * irot
                let !irot2 :: Int = AM.val irot2_
                let !irot3 :: Int = AM.val $ irot2_ * irot
                let !offset = s .<<. (h - len + 2)
                for_ [0 .. p - 1] $ \i -> do
                  !a0 :: Int <- AM.val <$> VGM.read a (i + offset + 0 * p)
                  !a1 :: Int <- AM.val <$> VGM.read a (i + offset + 1 * p)
                  !a2 :: Int <- AM.val <$> VGM.read a (i + offset + 2 * p)
                  !a3 :: Int <- AM.val <$> VGM.read a (i + offset + 3 * p)

                  let !a2na3iimag = (m + a2 - a3) * iimag `mod` m

                  VGM.write a (i + offset) . AM.new $! a0 + a1 + a2 + a3
                  VGM.write a (i + offset + 1 * p) . AM.new $! (a0 + (m - a1) + a2na3iimag) * irot1
                  VGM.write a (i + offset + 2 * p) . AM.new $! (a0 + a1 + (m - a2) + (m - a3)) * irot2
                  VGM.write a (i + offset + 3 * p) . AM.new $! (a0 + (m - a1) + (m - a2na3iimag)) * irot3
                if s + 1 /= bit (len - 2)
                  then pure . (irot *) $ iRate3Fft VG.! countTrailingZeros (complement s)
                  else pure irot
            )
            (AM.raw32 @p 1)
            (VU.generate (bit (len - 2)) id)
          loop $ len - 2

convolutionNaive ::
  forall p.
  (AM.Modulus p) =>
  VU.Vector (AM.StaticModInt p) ->
  VU.Vector (AM.StaticModInt p) ->
  VU.Vector (AM.StaticModInt p)
convolutionNaive a b = VU.create $ do
  let n = VU.length a
  let m = VU.length b
  ans <- VGM.replicate (n + m - 1) 0
  if n < m
    then do
      for_ [0 .. m - 1] $ \j -> do
        for_ [0 .. n - 1] $ \i -> do
          VGM.modify ans (+ a VG.! i * b VG.! j) (i + j)
    else do
      for_ [0 .. n - 1] $ \i -> do
        for_ [0 .. m - 1] $ \j -> do
          VGM.modify ans (+ a VG.! i * b VG.! j) (i + j)
  pure ans

convolutionFft ::
  (AM.Modulus p) =>
  VU.Vector (AM.StaticModInt p) ->
  VU.Vector (AM.StaticModInt p) ->
  VU.Vector (AM.StaticModInt p)
convolutionFft a_ b_ = VU.create $ do
  let n = VU.length a_
  let m = VU.length b_
  let z = ACIB.bitCeil (n + m - 1)
  a <- VUM.replicate z 0
  VU.iforM_ a_ $ \i ai -> do
    VGM.write a i ai
  butterfly a
  b <- VUM.replicate z 0
  VU.iforM_ b_ $ \i bi -> do
    VGM.write b i bi
  butterfly b
  VUM.iforM_ b $ \i bi -> do
    VGM.modify a (* bi) i
  butterflyInv a
  a' <-
    if n + m - 1 == z
      then pure a
      else do
        vec <- VUM.replicate (n + m - 1) 0
        VGM.copy vec $ VUM.take (n + m - 1) a
        pure vec
  let !iz = AM.inv $ AM.new z
  for_ [0 .. n + m - 2] $ \i -> do
    VGM.modify a' (* iz) i
  pure a'
