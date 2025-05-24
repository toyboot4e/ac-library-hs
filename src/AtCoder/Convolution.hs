{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}

-- | It calculates \((+,\times)\) convolution. Given two arrays \(a_0, a_1, \cdots, a_{N - 1}\) and \(b_0, b_1, \cdots, b_{M - 1}\), it calculates the array \(c\) of length \(N + M - 1\), defined by
--
-- \[
-- c_i = \sum_{j = 0}^i a_j b_{i - j}
-- \]
--
-- ==== __Example__
-- The convolution module basically works with `AtCoder.ModInt`:
--
-- >>> :set -XDataKinds
-- >>> import AtCoder.Convolution qualified as C
-- >>> import AtCoder.ModInt qualified as M
-- >>> import Data.Proxy (Proxy)
-- >>> import Data.Vector.Unboxed qualified as VU
--
-- Define specific modulus items:
--
-- >>> type Mint = M.ModInt998244353
-- >>> let modInt :: Int -> Mint; modInt = M.new
--
-- Calculate convolution:
--
-- >>> let a = VU.map modInt $ VU.fromList [1, 2, 3, 4]
-- >>> let b = VU.map modInt $ VU.fromList [5, 6, 7, 8]
-- >>> C.convolution a b
-- [5,16,34,60,61,52,32]
--
-- You can also target any @'Integral' a@ with `convolutionRaw`:
--
-- >>> let a = VU.fromList @Int [1, 2, 3, 4]
-- >>> let b = VU.fromList @Int [5, 6, 7, 8]
-- >>> C.convolutionRaw (Proxy @998244353) a b
-- [5,16,34,60,61,52,32]
--
-- If you want to calculate large values without taking mod, use `convolution64`.
--
-- @since 1.0.0.0
module AtCoder.Convolution
  ( -- * Convolution in mod m
    convolution,
    convolutionRaw,

    -- * Convolution
    convolution64,
  )
where

import AtCoder.Internal.Assert qualified as ACIA
import AtCoder.Internal.Bit qualified as ACIB
import AtCoder.Internal.Convolution qualified as ACIC
import AtCoder.Internal.Math qualified as ACIM
import AtCoder.ModInt qualified as AM
import Data.Bits (bit)
import Data.Proxy (Proxy (..))
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Exts (proxy#)
import GHC.Stack (HasCallStack)
import GHC.TypeNats (natVal')

-- NOTE(pref): Inlining `convolution` functions makes the compile time extremely too slow, while it
-- surely makes better runtime performance (#51).

-- | Calculates the convolution in \(\bmod m\) for a vector of `ACIM.ModInt`. It returns an empty
-- array if at least one of \(a\) and \(b\) are empty.
--
-- ==== Constraints
-- - \(2 \leq m \leq 2 \times 10^9\)
-- - \(m\) is prime.
-- - There is an integer \(c\) with \(2^c | (m - 1)\) and \(|a| + |b| - 1 \leq 2^c\).
--
-- ==== Complexity
-- - \(O(n\log{n} + \log{\mathrm{mod}})\), where \(n = |a| + |b|\).
--
-- @since 1.0.0.0
{-# INLINEABLE convolution #-}
convolution ::
  forall p.
  (HasCallStack, AM.Modulus p) =>
  VU.Vector (AM.ModInt p) ->
  VU.Vector (AM.ModInt p) ->
  VU.Vector (AM.ModInt p)
convolution a b
  | n == 0 || m == 0 = VU.empty
  | otherwise =
      let z = ACIB.bitCeil (n + m - 1)
          !modulus = fromIntegral (natVal' (proxy# @p))
          !_ = ACIA.runtimeAssert ((modulus - 1) `mod` z == 0) $ "AtCoder.Convolution.convolution: not works when `(m - 1) mod z /= 0`: " ++ show (m, z)
       in if min n m <= 60
            then ACIC.convolutionNaive a b
            else ACIC.convolutionFft a b
  where
    n = VU.length a
    m = VU.length b

-- | Calculates `convolution` in \(\bmod m\) for any `Integral` @a@.
--
-- ==== Constraints
-- - \(2 \leq m \leq 2 \times 10^9\)
-- - \(m\) is prime.
-- - There is an integer \(c\) with \(2^c | (m - 1)\) and \(|a| + |b| - 1 \leq 2^c\).
--
-- ==== Complexity
-- - \(O(n\log{n} + \log{\mathrm{mod}})\), where \(n = |a| + |b|\).
--
-- @since 1.0.0.0
{-# INLINEABLE convolutionRaw #-}
convolutionRaw ::
  forall p a.
  (HasCallStack, AM.Modulus p, Integral a, VU.Unbox a) =>
  Proxy p ->
  VU.Vector a ->
  VU.Vector a ->
  VU.Vector a
convolutionRaw _ a b
  | n == 0 || m == 0 = VU.empty
  | otherwise =
      let z = ACIB.bitCeil (n + m - 1)
          !modulus = fromIntegral (natVal' (proxy# @p))
          !_ = ACIA.runtimeAssert ((modulus - 1) `mod` z == 0) $ "AtCoder.Convolution.convolutionRaw: not works when `(m - 1) mod z /= 0`: " ++ show (m, z)
       in -- `Vector a` -> `Vector (ModInt p)` -> `Vector a`
          VU.map fromIntegral $ convolution @p (VU.map fromIntegral a) (VU.map fromIntegral b)
  where
    n = VU.length a
    m = VU.length b

-- | Calculates the convolution (without taking mod). It returns an empty array if at least one of
-- \(a\) and \(b\) are empty.
--
-- ==== Constraints
-- - \(|a| + |b| - 1 \leq 2^{24}\)
--
-- ==== Complexity
-- - \(O(n\log{n})\), where \(n = |a| + |b|\).
--
-- @since 1.0.0.0
{-# INLINEABLE convolution64 #-}
convolution64 ::
  (HasCallStack) =>
  VU.Vector Int ->
  VU.Vector Int ->
  VU.Vector Int
convolution64 a b
  | n == 0 || m == 0 = VU.empty
  | otherwise =
      let mod1 :: Int = 754974721 -- 2^24
          mod2 :: Int = 167772161 -- 2^25
          mod3 :: Int = 469762049 -- 2^26
          m2m3 :: Int = mod2 * mod3
          m1m3 :: Int = mod1 * mod3
          m1m2 :: Int = mod1 * mod2
          m1m2m3 :: Int = mod1 * mod2 * mod3

          (!_, !i1) = ACIM.invGcd (mod2 * mod3) mod1
          (!_, !i2) = ACIM.invGcd (mod1 * mod3) mod2
          (!_, !i3) = ACIM.invGcd (mod1 * mod2) mod3

          maxAbBit = 24
          -- static assertions are commented out:
          -- !_ = ACIA.runtimeAssert (mod1 `mod` bit maxAbBit == 1) $ "AtCoder.Convolution.convolution64: `mod1` isn't enough to support an array of length `2^24`."
          -- !_ = ACIA.runtimeAssert (mod2 `mod` bit maxAbBit == 1) $ "AtCoder.Convolution.convolution64: `mod2` isn't enough to support an array of length `2^25`."
          -- !_ = ACIA.runtimeAssert (mod3 `mod` bit maxAbBit == 1) $ "AtCoder.Convolution.convolution64: `mod3` isn't enough to support an array of length `2^26`."
          !_ = ACIA.runtimeAssert (n + m - 1 <= bit maxAbBit) "AtCoder.Convolution.convolution64: given too long vector as input"
          c1 = convolution {- mod1 -} (VU.map (AM.new @754974721) a) (VU.map (AM.new @754974721) b)
          c2 = convolution {- mod2 -} (VU.map (AM.new @167772161) a) (VU.map (AM.new @167772161) b)
          c3 = convolution {- mod3 -} (VU.map (AM.new @469762049) a) (VU.map (AM.new @469762049) b)
       in VU.create $ do
            c <- VUM.unsafeNew (n + m - 1)
            let !offset = VU.fromListN @Int 5 [0, 0, m1m2m3, 2 * m1m2m3, 3 * m1m2m3]
            VU.ifoldM'_
              ( \ !x i (AM.ModInt !x1, AM.ModInt !x2, AM.ModInt !x3) -> do
                  let !x' =
                        x
                          + (fromIntegral x1 * i1) `mod` mod1 * m2m3
                          + (fromIntegral x2 * i2) `mod` mod2 * m1m3
                          + (fromIntegral x3 * i3) `mod` mod3 * m1m2
                  let diff = fromIntegral x1 - x' `mod` mod1
                  let diff' = if diff < 0 then diff + mod1 else diff
                  let !x'' = x' - offset VG.! (diff' `mod` 5)
                  VGM.write c i x''
                  pure x
              )
              (0 :: Int)
              $ VU.zip3 c1 c2 c3
            pure c
  where
    n = VU.length a
    m = VU.length b
