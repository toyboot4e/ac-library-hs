{-# LANGUAGE TypeFamilies #-}

-- | Math module. It contains number-theoretic algorithms.
--
-- @since 1.0.0.0
module AtCoder.Math
  ( -- * Modulus operations

    -- These functions are internally used for `AtCoder.ModInt`.
    powMod,
    invMod,

    -- * Chinese Remainder Theorem
    crt,

    -- * Floor sum
    floorSum,
  )
where

import AtCoder.Internal.Assert qualified as ACIA
import AtCoder.Internal.Math (powMod)
import AtCoder.Internal.Math qualified as ACIM
import Data.Bits (bit)
import Data.Vector.Unboxed qualified as VU
import GHC.Stack (HasCallStack)

-- `powMod` is re-exported from the internal math module.

-- | Returns an integer \(y\) such that \(0 \le y < m\) and  \(xy \equiv 1 \pmod m\).
--
-- ==== Constraints
-- - \(\gcd(x, m) = 1\)
-- - \(1 \leq m\)
--
-- ==== Complexity
-- - \(O(\log m)\)
--
-- ==== Example
-- >>> let m = 998244353
-- >>> (invMod 2 m) * 2 `mod` m -- (2^(-1) mod m) * 2 mod m
-- 1
--
-- @since 1.0.0.0
{-# INLINE invMod #-}
invMod ::
  (HasCallStack) =>
  -- | \(x\)
  Int ->
  -- | \(m\)
  Int ->
  -- | \(x^{-1} \bmod m\)
  Int
invMod x m =
  let !_ = ACIA.runtimeAssert (1 <= m) $ "AtCoder.Math.invMod: given invalid `m` less than 1: " ++ show m
      (!z1, !z2) = ACIM.invGcd (fromIntegral x) (fromIntegral m)
      !_ = ACIA.runtimeAssert (z1 == 1) "AtCoder.Math.invMod: `x^(-1) mod m` cannot be calculated when `gcd x m /= 1`"
   in z2

-- | Given two arrays \(r,m\) with length \(n\), it solves the modular equation system
--
-- \[
-- x \equiv r[i] \pmod{m[i]}, \forall i \in \lbrace 0,1,\cdots, n - 1 \rbrace.
-- \]
--
-- If there is no solution, it returns \((0, 0)\). Otherwise, all the solutions can be written as the form \(x \equiv y \pmod z\), using integers
-- \(y, z\) \((0 \leq y < z = \mathrm{lcm}(m[i]))\). It returns this \((y, z)\) as a pair. If \(n=0\), it returns \((0, 1)\).
--
-- ==== Constraints
-- - \(|r| = |m|\)
-- - \(1 \le m[i]\)
-- - \(\mathrm{lcm}(m[i])\) is in `Int` bounds.
--
-- ==== Complexity
-- - \(O(n \log{\mathrm{lcm}(m[i])})\)
--
-- ==== __Example__
-- `crt` calculates \(y\) such that \(y \equiv r_i \pmod m_i, \forall i \in \lbrace 0,1,\cdots, n - 1 \rbrace\):
--
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> let rs = VU.fromList @Int [6, 7, 8, 9]
-- >>> let ms = VU.fromList @Int [2, 3, 4, 5]
-- >>> crt rs ms
-- (4,60)
--
-- The property can be checked as follows:
--
-- >>> let (y, {- lcm ms -} _) = crt rs ms
-- >>> VU.zipWith mod rs ms
-- [0,1,0,4]
--
-- >>> VU.zipWith mod rs ms == VU.map (y `mod`) ms
-- True
--
-- @since 1.0.0.0
{-# INLINE crt #-}
crt :: (HasCallStack) => VU.Vector Int -> VU.Vector Int -> (Int, Int)
crt r m = loop 0 1 [0 .. VU.length r - 1]
  where
    !_ = ACIA.runtimeAssert (VU.length r == VU.length m) "AtCoder.Math.crt: given `r` and `m` with different lengths"
    loop !r0 !m0 [] = (r0, m0)
    loop !r0 !m0 (i : rest)
      | m0' `mod` m1' == 0 =
          if r0' `mod` m1' /= r1'
            then (0, 0)
            else loop r0' m0' rest
      | otherwise =
          let (!g, !im) = ACIM.invGcd m0' m1'
              u1 = m1' `div` g
           in if ((r1' - r0') `mod` g) /= 0
                then (0, 0)
                else
                  let !x = (r1' - r0') `div` g `mod` u1 * im `mod` u1
                      !r0'' = r0' + x * m0'
                      !m0'' = m0' * u1
                   in if r0'' < 0
                        then loop (r0'' + m0'') m0'' rest
                        else loop r0'' m0'' rest
      where
        !_ = ACIA.runtimeAssert (1 <= mi) $ "AtCoder.Math.crt: `m[i]` is not positive: " ++ show mi
        !mi = VU.unsafeIndex m i
        !ri = VU.unsafeIndex r i
        !r1 = ri `mod` mi
        !m1 = mi
        (!m0', !m1', !r0', !r1')
          | m0 < m1 = (m1, m0, r1, r0)
          | otherwise = (m0, m1, r0, r1)

-- | Returns \(\sum\limits_{i = 0}^{n - 1} \left\lfloor \frac{a \times i + b}{m} \right\rfloor\).
--
-- ==== Constraints
-- - \(0 \le n\)
-- - \(1 \le m\)
--
-- ==== Complexity
-- - \(O(\log m)\)
--
-- ==== __Example__
-- `floorSum` calculates the number of points surrounded by a line
-- \(y = \frac {a \times x + b} {m} \) and \(x, y\) axes in \(O(\log m)\) time:
--
-- >>> floorSum 5 1 1 1 -- floorSum n {- line information -} m a b
-- 15
--
-- @
--   y
--   ^
-- 6 |
-- 5 |           o           line: y = x + 1
-- 4 |        o  o           The number of \`o\` is 15
-- 3 |     o  o  o
-- 2 |  o  o  o  o
-- 1 |  o  o  o  o
-- --+-----------------> x
--   0  1  2  3  4  5
--                  n = 5
-- @
--
-- @since 1.0.0.0
{-# INLINE floorSum #-}
floorSum ::
  (HasCallStack) =>
  -- | \(n\)
  Int ->
  -- | \(m\)
  Int ->
  -- | \(a\)
  Int ->
  -- | \(b\)
  Int ->
  -- | \(\sum\limits_{i = 0}^{n - 1} \left\lfloor \frac{a \times i + b}{m} \right\rfloor\)
  Int
floorSum n m a b = ACIM.floorSumUnsigned n m a' b' - da - db
  where
    !_ = ACIA.runtimeAssert (0 <= n && n < bit 32) $ "AtCoder.Math.floorSum: given invalid `n` (`" ++ show n ++ "`)"
    !_ = ACIA.runtimeAssert (1 <= m && m < bit 32) $ "AtCoder.Math.floorSum: given invalid `m` (`" ++ show m ++ "`)"
    a'
      | a < 0 = a `mod` m
      | otherwise = a
    da
      | a < 0 = n * (n - 1) `div` 2 * (((a `mod` m) - a) `div` m)
      | otherwise = 0
    b'
      | b < 0 = b `mod` m
      | otherwise = b
    db
      | b < 0 = n * (((b `mod` m) - b) `div` m)
      | otherwise = 0
