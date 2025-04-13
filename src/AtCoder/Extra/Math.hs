-- | Extra math module.
--
-- @since 1.0.0.0
module AtCoder.Extra.Math
  ( -- * Re-exports from the internal math module
    isPrime32,
    ACIM.invGcd,
    primitiveRoot32,

    -- * Prime numbers
    primes,
    isPrime,
    primeFactors,
    primeFactorsUnsorted,

    -- * Binary exponentiation

    -- | ==== __Examples__
    -- >>> import AtCoder.Extra.Math qualified as M
    -- >>> import Data.Semigroup (Product(..), Sum(..))
    -- >>> getProduct $ M.power (<>) 32 (Product 2)
    -- 4294967296
    --
    -- >>> getProduct $ M.stimes' 32 (Product 2)
    -- 4294967296
    --
    -- >>> getProduct $ M.mtimes' 32 (Product 2)
    -- 4294967296
    power,
    stimes',
    mtimes',
  )
where

import AtCoder.Extra.Math.Montgomery64 qualified as M64
import AtCoder.Internal.Assert qualified as ACIA
import AtCoder.Internal.Math qualified as ACIM
import Control.Monad (unless, when)
import Data.Bit (Bit (..))
import Data.Bits (bit, countTrailingZeros, (.<<.), (.>>.))
import Data.Foldable (for_)
import Data.Maybe (fromJust)
import Data.Vector.Algorithms.Intro qualified as VAI
import Data.Vector.Algorithms.Radix qualified as VAR
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import Data.Word (Word64)
import GHC.Stack (HasCallStack)
import System.Random

-- | \(O(k \log^3 n) (k = 3)\). Returns whether the given `Int` value is a prime number.
--
-- ==== Constraints
-- - \(n < 4759123141 (2^{32} < 4759123141)\), otherwise the return value can lie
--   (see [Wikipedia](https://en.wikipedia.org/wiki/Miller%E2%80%93Rabin_primality_test#Testing_against_small_sets_of_bases)).
--
--
-- @since 1.1.0.0
{-# INLINE isPrime32 #-}
isPrime32 :: (HasCallStack) => Int -> Bool
isPrime32 x = ACIM.isPrime x
  where
    !_ = ACIA.runtimeAssert (x < 4759123141) $ "AtCoder.Extra.Math.isPrime32: given too large number `" ++ show x ++ "`"

-- | Returns the primitive root of the given `Int`.
--
-- ==== Constraints
-- - The input must be a prime number.
-- - The input must be less than \(2^32\).
--
-- @since 1.2.0.0
{-# INLINE primitiveRoot32 #-}
primitiveRoot32 :: (HasCallStack) => Int -> Int
primitiveRoot32 x = ACIM.primitiveRoot x
  where
    !_ = ACIA.runtimeAssert (x < (1 .>>. 32)) $ "AtCoder.Extra.Math.primitiveRoot32: given too large number `" ++ show x ++ "`"

-- | \(O(n \log \log n)\) Creates an array of prime numbers up to the given limit, using Sieve of
-- Eratosthenes.
--
-- The minimum computational complexity is \(\Omega(B \log \log B)\), where \(B = 2^{15}\) is the
-- length of segment. This constraint comes from the use of segmented sieve.
--
-- ==== Constraints
-- - The upper limit must be less than or equal to \(2^{30} (\gt 10^9)\), otherwise the returned
-- prime table is incorrect.
--
-- @since 1.2.6.0
{-# INLINEABLE primes #-}
primes :: Int -> VU.Vector Int
primes upperLimit
  | upperLimit <= 1 = VU.empty
  | otherwise = VU.create $ do
      -- segment length (TODO: isn't it 32767?)
      let !s = 32768 :: Int -- 2 ^ 15

      -- sieve length (TODO: use \sqrt limit? do benchmark)
      let !sieveMax = s

      -- Is it like LT bound??
      let !limit = upperLimit + 1

      -- base primes with index
      (!ps, !is) <- do
        sieve <- VUM.replicate (sieveMax + 1) $ Bit False
        ps <- VUM.unsafeNew (sieveMax `div` 2)
        is <- VUM.unsafeNew (sieveMax `div` 2)
        -- FIXME: carry index?
        iNext <- VUM.replicate 1 (0 :: Int)
        for_ [3, 5 .. s] $ \p1 -> do
          Bit b <- VGM.read sieve p1
          unless b $ do
            at <- VGM.read iNext 0
            VGM.write iNext 0 $ at + 1
            -- (base prime, next index (odd numbers only, so `div` 2)
            VGM.write ps at p1
            VGM.write is at $ p1 * p1 `div` 2
            -- NOTE: if `j` is a composite number, it's already enumerated by a smaller prime
            -- number than `p0`, so skip to `p1 * p1` and iterate through odd numbers only:
            for_ [p1 * p1, p1 * p1 + 2 * p1 .. sieveMax] $ \np1 -> do
              VGM.write sieve np1 $ Bit True
        len <- VGM.read iNext 0
        (,VGM.take len is) <$> VU.unsafeFreeze (VGM.take len ps)

      -- https://en.wikipedia.org/wiki/Prime-counting_function
      let !maxPrimeCount :: Int
            -- NOTE: 1,700 is a point where the next function estimates better as far as I tested:
            | limit < 1700 = round (1.25506 * fromIntegral limit / log (fromIntegral limit) :: Double)
            -- Rosser and Schoenfeld Boundsh (1962): holds for x > e^{3/2}:
            | limit < 60184 = round (fromIntegral limit / (log (fromIntegral limit) - 1.5) :: Double)
            -- Pierre Dusart (2010): holds for x >= 60184:
            | otherwise = ceiling (fromIntegral limit / (log (fromIntegral limit) - 1.1) :: Double)

      -- let f x = round (1.25506 * fromIntegral x / log (fromIntegral x) :: Double)
      -- let g x = round (fromIntegral x / (log (fromIntegral x) - 1.5) :: Double)
      -- let h x = ceiling (fromIntegral x / (log (fromIntegral x) - 1.1) :: Double)
      -- let p x = (f x, g x, h x)

      result <- VUM.replicate maxPrimeCount (-1)
      VGM.write result 0 2
      -- FIXME: carry index?
      nPrimes <- VUM.replicate 1 (1 :: Int)

      -- Sieve of Eratosthenes by block of size `s`, ignoring even numers
      -- FIXME: block length of size `s/2` should make more sense?
      block <- VUM.unsafeNew s
      let !r = limit `div` 2
      for_ [1, 1 + s .. r] $ \l -> do
        VGM.set block $ Bit False

        VU.iforM_ ps $ \idx p -> do
          -- FIXME: cut out the ps beforehand
          when (p <= limit) $ do
            i0 <- VGM.read is idx
            let run i = do
                  if i < l + s
                    then do
                      -- within the block
                      VGM.write block (i - l) $ Bit True
                      run $ i + p
                    else do
                      -- went out of the block
                      VGM.write is idx i
            run i0

        block' <- VU.take (min s (r - l)) <$> VU.unsafeFreeze block
        VU.iforM_ block' $ \i (Bit b) -> do
          unless b $ do
            at <- VGM.read nPrimes 0
            when (at < maxPrimeCount) $ do
              VGM.write nPrimes 0 $ at + 1
              VGM.write result at $ (l + i) * 2 + 1

      len <- VGM.read nPrimes 0
      pure $ VGM.take len result

-- | \(O(w \log^3 n)\) Miller–Rabin primality test, where \(w = 3\) for \(x \lt 2^{32}\) and
-- \(w = 7\) for \(x \ge 3^{32}\).
--
-- @since 1.2.6.0
{-# INLINEABLE isPrime #-}
isPrime :: Int -> Bool
isPrime x
  | x <= 1 = False
  -- Up to 11^2:
  | x == 2 || x == 3 || x == 5 || x == 7 = True
  | even x || x `rem` 3 == 0 || x `rem` 5 == 0 || x `rem` 7 == 0 = False
  | x < 121 = True
isPrime x
  -- http://miller-rabin.appspot.com/
  -- \| x < bit 32 = all test [2, 7, 61]
  | x < bit 32 = test 2 && test 7 && test 61
  -- \| otherwise = all test [2, 325, 9375, 28178, 450775, 9780504, 1795265022]
  | otherwise = test 2 && test 325 && test 9375 && test 28178 && test 450775 && test 9780504 && test 1795265022
  where
    !x64 :: Word64 = fromIntegral x
    !d :: Word64 = (x64 - 1) .>>. countTrailingZeros (x64 - 1)
    !mont = M64.fromVal x64
    !one = M64.encode mont 1
    !minusOne = M64.encode mont (x64 - 1)
    test a = inner (M64.powMod mont (M64.encode mont a) (fromIntegral d)) d
      where
        inner :: Word64 -> Word64 -> Bool
        inner y t
          | not (M64.eq x64 y one) && not (M64.eq x64 y minusOne) && t /= x64 - 1 = inner (M64.mulMod mont y y) (t .<<. 1)
          | not (M64.eq x64 y minusOne) && even t = False
          | otherwise = True

-- | Pollard's Rho algorithm.
{-# INLINEABLE rho #-}
rho :: (HasCallStack) => Word64 -> Int -> Int -> Int
rho modVal n c
  | n < 1 = error $ "AtCoder.Extra.Math.rho: given value less than or equal to `1`: `" ++ show n ++ show "`"
  | otherwise = fromIntegral $! inner 1 (M64.encode mont 1) (M64.encode mont 2) (M64.encode mont 1) (M64.encode mont 1) 1
  where
    -- what a mess!!
    !mont = M64.fromVal modVal
    !n64 :: Word64 = fromIntegral n
    !cc = M64.encode mont $ fromIntegral c
    f !x = M64.addMod modVal (M64.mulMod mont x x) cc
    fn 0 !x = x
    fn n_ !x = fn (n_ - 1) $! f x
    !m2 :: Int = bit $ floor (logBase (2.0 :: Double) (fromIntegral n)) `div` 5
    inner r _lastY0 y0 z0 q0 g0
      | g0 == 1 =
          let !y = fn r y0
              (!y', !z', !q', !g') = inner2 0 y z0 q0 g0
           in inner (r .<<. 1) y0 y' z' q' g'
      -- FIXME: It can sometimes slow, depending on the seed value
      -- \| g0 == n64 = inner3 z0
      | otherwise = g0
      where
        inner2 !k !y !z !q !g
          | k >= r || g /= 1 = (y, z, q, g)
          | otherwise =
              let (!y', !q') = fn2 (min m2 (r - k)) y q
                  !g' = gcd (M64.decode mont q) n64
               in inner2 (k + m2) y' y q' g'
          where
            fn2 0 !y_ !q_ = (y_, q_)
            fn2 n_ !y_ !q_ =
              let !y' = f y_
                  !q' = M64.mulMod mont q_ (M64.subMod modVal y0 y')
               in fn2 (n_ - 1) y' q'

-- FIXME: it can sometimes slow, depending on the seed value
-- inner3 !z
--   | g == 1 = inner3 z'
--   | otherwise = g
--   where
--     !z' = f z
--     !g = gcd (M64.decode mont (M64.subMod modVal lastY0 z')) n64

-- | Tries to find a prime factor for the given value, running Pollard's Rho algorithm.
--
-- ==== Constrants
-- - \(x \gt 1\)
{-# INLINEABLE findPrimeFactor #-}
findPrimeFactor :: (HasCallStack) => StdGen -> Int -> (Maybe Int, StdGen)
findPrimeFactor gen0 n
  | n <= 1 = error $ "AtCoder.Extra.Math.findPrimeFactor: given value less than or equal to `1`: " ++ show n ++ show "`"
  | isPrime n = (Just n, gen0)
  | otherwise = tryN 200 gen0
  where
    tryN :: Int -> StdGen -> (Maybe Int, StdGen)
    tryN 0 gen = (Nothing, gen)
    tryN i gen
      | isPrime m = (Just m, gen')
      | otherwise = tryN (i - 1) gen'
      where
        (!rnd, !gen') = uniformR (0, n - 1) gen
        !m = rho (fromIntegral n) n rnd

-- | Returns prime factors in run-length encoding \((p_i, n_i)\), sorted by \(p_i\).
--
-- ==== Constraints
-- - \(x \ge 1\)
--
-- @since 1.2.6.0
{-# INLINE primeFactors #-}
primeFactors :: (HasCallStack) => Int -> VU.Vector (Int, Int)
primeFactors = VU.modify VAI.sort . primeFactorsUnsorted

-- | Returns prime factors in run-length encoding \((p_i, n_i)\) in arbitrary order.
--
-- It internally uses probabilistic method (Pollard's rho algorithm) and it can actually result in
-- runtime error, however, the probability is very low and the API does not return `Maybe`.
--
-- @since 1.2.6.0
{-# INLINEABLE primeFactorsUnsorted #-}
primeFactorsUnsorted :: (HasCallStack) => Int -> VU.Vector (Int, Int)
primeFactorsUnsorted n
  | n < 1 = error $ "AtCoder.Extra.Math.primeFactorsUnsorted: given non-positive value `" ++ show n ++ "`"
  | otherwise = VU.create $ do
      buf <- VUM.unsafeNew (ceiling (logBase (2 :: Double) (fromIntegral n)))

      -- for small prime factors, try them all:
      let runDiv cur iWrite [] = pure (cur, iWrite)
          runDiv cur iWrite (d : rest)
            | d * d > cur = pure (cur, iWrite)
            | otherwise = case tryDiv 0 cur d of
                Just (!cur', !nd) -> do
                  VGM.write buf iWrite (d, nd)
                  runDiv cur' (iWrite + 1) rest
                Nothing -> runDiv cur iWrite rest

      (!n', !iWrite0) <- runDiv n 0 (2 : [3, 5 .. 97])

      -- for bigger prime numbers, use Polland's rho algorithm:
      let runRho !gen !cur !iWrite
            | cur > 1 = case findPrimeFactor gen cur of
                (Just p, !gen') -> do
                  let (!cur', !np) = fromJust $ tryDiv 0 cur p
                  VGM.write buf iWrite (p, np)
                  runRho gen' cur' (iWrite + 1)
                (Nothing, !_gen') -> do
                  -- we could return `Nothing` instead
                  error $ "unable to find prime factor for " ++ show cur
            | otherwise = pure iWrite

      -- NOTE: The seed value ifs fixed here. We could decide it at runtime for possibly faster
      -- submissions on TLE redjuge, however, 're rather preferring deterministic result:
      len <- runRho (mkStdGen 123456789) n' iWrite0
      pure $ VUM.take len buf
  where
    tryDiv :: Int -> Int -> Int -> Maybe (Int, Int)
    tryDiv !nDiv x d
      | r == 0 = tryDiv (nDiv + 1) q d
      | nDiv > 0 = Just (x, nDiv)
      | otherwise = Nothing
      where
        (!q, !r) = x `quotRem` d

-- | Calculates \(x^n\) with custom multiplication operator using the binary exponentiation
-- technique.
--
-- The internal implementation is taken from @Data.Semigroup.stimes@, but `power` uses strict
-- evaluation and is often much faster.
--
-- ==== Complexity
-- - \(O(\log n)\)
--
-- ==== Constraints
-- - \(n \gt 0\)
--
-- @since 1.0.0.0
{-# INLINE power #-}
power :: (a -> a -> a) -> Int -> a -> a
power op n0 x1
  | n0 <= 0 = errorWithoutStackTrace "AtCoder.Extra.Math.power: positive multiplier expected"
  | otherwise = f x1 n0
  where
    f !x !n
      | even n = f (x `op` x) (n .>>. 1)
      | n == 1 = x
      | otherwise = g (x `op` x) (n .>>. 1) x
    g !x !n !z
      | even n = g (x `op` x) (n .>>. 1) z
      | n == 1 = x `op` z
      | otherwise = g (x `op` x) (n .>>. 1) (x `op` z)

-- | Strict variant of @Data.Semigroup.stimes@.
--
-- ==== Complexity
-- - \(O(\log n)\)
--
-- ==== Constraints
-- - \(n \gt 0\)
--
-- @since 1.0.0.0
{-# INLINE stimes' #-}
stimes' :: (Semigroup a) => Int -> a -> a
stimes' = power (<>)

-- | Strict variant of @Data.Monoid.mtimes@.
--
-- ==== Complexity
-- - \(O(\log n)\)
--
-- ==== Constraints
-- - \(n \ge 0\)
--
-- @since 1.0.0.0
{-# INLINE mtimes' #-}
mtimes' :: (Monoid a) => Int -> a -> a
mtimes' n x = case compare n 0 of
  LT -> errorWithoutStackTrace "AtCoder.Extra.Math.mtimes': non-negative multiplier expected"
  EQ -> mempty
  GT -> power (<>) n x
