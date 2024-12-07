module AtCoder.Internal.Math
  ( powMod,
    isPrime,
    invGcd,
    primitiveRoot,
    floorSumUnsigned,
  )
where

import AtCoder.Internal.Assert qualified as ACIA
import AtCoder.Internal.Barrett qualified as ACIBT
import Control.Monad.ST (runST)
import Data.Bits ((.<<.), (.>>.))
import Data.Foldable
import Data.Maybe (fromJust)
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import Data.Word (Word32, Word64)
import GHC.Stack (HasCallStack)

-- safeMod :: Int -> Int -> Int
-- safeMod = mod

-- | Returns \(x^n \bmod m\).
--
-- = Constraints
-- - \(0 \le n\)
-- - \(1 \le m\)
--
-- = Complexity
-- - \(O(\log n)\)
powMod :: (HasCallStack) => Int -> Int -> Int -> Int
powMod x n0 m0
  | m0 == 1 = 0
  | otherwise = fromIntegral $ inner n0 1 $ fromIntegral (x `mod` m0)
  where
    !_ = ACIA.runtimeAssert (0 <= n0 && 1 <= m0) $ "AtCoder.Internal.Math.powMod: given invalid `n` or `m`: " ++ show (n0, m0)
    bt = ACIBT.new $ fromIntegral m0
    inner :: Int -> Word32 -> Word32 -> Word32
    inner !n !r !y
      | n == 0 = r
      | otherwise =
          let r' = if odd n then ACIBT.mul bt r y else r
              y' = ACIBT.mul bt y y
           in inner (n .>>. 1) r' y'

-- | M. Forisek and J. Jancina, Fast Primality Testing for Integers That Fit into a Machine Word
isPrime :: Int -> Bool
isPrime n
  | n <= 1 = False
  | n == 2 || n == 7 || n == 61 = True
  | even n = False
  | otherwise =
      let d = innerD $ n - 1
          test a = inner d $ powMod a d n
       in all test [2, 7, 61 :: Int]
  where
    innerD d
      | even d = innerD $ d `div` 2
      | otherwise = d
    inner t y
      | t == n - 1 || y == 1 || y == n - 1 = not $ y /= n - 1 && even t
      | otherwise = inner (t .<<. 1) (y * y `mod` n)

-- | Returns @(g, x)@ such that \(g = \gcd(a, b), \mathrm{xa} = g(\bmod b), 0 \le x \le b/g\).
--
-- = Constraints
-- - \(1 \le b\) (not asserted)
invGcd :: Int -> Int -> (Int, Int)
invGcd a0 b
  | a == 0 = (b, 0)
  | otherwise = inner b a 0 1
  where
    !a = a0 `mod` b
    -- Contracts:
    -- [1] s - m0 * a = 0 (mod b)
    -- [2] t - m1 * a = 0 (mod b)
    -- [3] s * |m1| + t * |m0| <= b
    inner :: Int -> Int -> Int -> Int -> (Int, Int)
    inner !s !t !m0 !m1
      | t == 0 =
          let !m' = if m0 < 0 then m0 + b `div` s else m0
           in (s, m')
      | otherwise =
          let !u = s `div` t
              !s' = s - t * u
              !m0' = m0 - m1 * u
           in inner t s' m1 m0'

-- | Returns primitive root.
primitiveRoot :: Int -> Int
primitiveRoot m
  | m == 2 = 1
  | m == 167772161 = 3
  | m == 469762049 = 3
  | m == 754974721 = 11
  | m == 998244353 = 3
  | otherwise = runST $ do
      let divs_ = VU.create $ do
            divs <- VUM.replicate 20 (0 :: Int)
            VGM.write divs 0 2
            let innerX x
                  | even x = innerX $ x `div` 2
                  | otherwise = x
            let inner !i !x !cnt
                  | (fromIntegral i :: Word64) * fromIntegral i > fromIntegral x = pure (x, cnt)
                  | x `mod` i == 0 = do
                      VGM.write divs cnt i
                      let loop x'
                            | x' `mod` i == 0 = loop (x' `div` i)
                            | otherwise = x'
                      inner (i + 2) (loop x) (cnt + 1)
                  | otherwise = inner (i + 2) x cnt
            (!x, !cnt) <- inner 3 (innerX ((m - 1) `div` 2)) 1
            !cnt' <- do
              if x > 1
                then do
                  VGM.write divs cnt x
                  pure $ cnt + 1
                else pure cnt
            pure $ VUM.take cnt' divs
      let test g = VU.all (testG g) divs_
          testG g divsI = powMod g ((m - 1) `div` divsI) m /= 1
      pure . fromJust $ find test [2 ..]

-- | \(O(\log m)\)
--
-- = Constraints
-- - \(n \lt 2^{32}\)
-- - \(1 \le m \lt 2^{32}\)
floorSumUnsigned :: Int -> Int -> Int -> Int -> Int
floorSumUnsigned = inner 0
  where
    inner acc n m a b
      | yMax < m = acc'
      | otherwise = inner acc' (yMax `div` m) a' m (yMax `rem` m)
      where
        a'
          | a >= m = a `rem` m
          | otherwise = a
        b'
          | b >= m = b `rem` m
          | otherwise = b
        da
          | a >= m = n * (n - 1) `div` 2 * (a `div` m)
          | otherwise = 0
        db
          | b >= m = n * (b `div` m)
          | otherwise = 0
        acc' = acc + da + db
        yMax = a' * n + b'