-- | Extra math module.
--
-- @since 1.0.0.0
module AtCoder.Extra.Math
  ( -- * Prime numbers and divisors
    primes,
    isPrime,
    primeFactors,
    primeFactorsUnsorted,
    divisors,
    divisorsUnsorted,

    -- * PrimitiveRoot
    primitiveRoot,

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
-- ==== __Example__
-- >>> primes 100
-- [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]
--
-- @since 1.2.6.0
{-# INLINEABLE primes #-}
primes :: (HasCallStack) => Int -> VU.Vector Int
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
            -- Rosser and Schoenfeld Bounds (1962): holds for x > e^{3/2}:
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

      -- Sieve of Eratosthenes by block of size `s`, ignoring even numbers
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
-- \(w = 7\) for \(x \ge 2^{32}\).
--
-- ==== __Example__
-- >>> isPrime 100055128505716009
-- True
--
-- @since 1.2.6.0
{-# INLINEABLE isPrime #-}
isPrime :: (HasCallStack) => Int -> Bool
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
-- ==== Constraints
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
-- ==== __Example__
-- >>> primeFactors 180
-- [(2,2),(3,2),(5,1)]
--
-- >>> primeFactors 123123123123123123
-- [(3,2),(7,1),(11,1),(13,1),(19,1),(41,1),(52579,1),(333667,1)]
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

      -- NOTE: The seed value is fixed here. We could decide it at runtime for possibly faster
      -- submissions on TLE redjuge, however, we're rather preferring deterministic result:
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

-- | Enumerates divisors of the input value.
--
-- ==== Constraints
-- - \(x \ge 1\)
--
-- ==== __Example__
-- >>> divisors 180
-- [1,2,3,4,5,6,9,10,12,15,18,20,30,36,45,60,90,180]
--
-- @since 1.2.6.0
{-# INLINE divisors #-}
divisors :: (HasCallStack) => Int -> VU.Vector Int
-- TODO: use intro sort?
divisors = VU.modify VAR.sort . divisorsUnsorted

-- | Enumerates divisors of the input value, not sorted.
--
-- ==== Constraints
-- - \(x \ge 1\)
--
-- ==== __Example__
-- >>> divisorsUnsorted 180
-- [1,2,4,3,6,12,9,18,36,5,10,20,15,30,60,45,90,180]
--
-- @since 1.2.6.0
{-# INLINEABLE divisorsUnsorted #-}
divisorsUnsorted :: (HasCallStack) => Int -> VU.Vector Int
divisorsUnsorted x = VU.create $ do
  vec <- VUM.unsafeNew nDivisors
  VGM.write vec 0 1
  VU.foldM'_
    ( \lenSofar (!p, !np) -> do
        (fst <$>)
          $ VU.foldM'
            ( \(!offset, !pp) _ -> do
                let !pp' = pp * p
                -- multiply to all the values sofar:
                VGM.iforM_ (VGM.take lenSofar vec) $ \i vx -> do
                  VGM.write vec (offset + i) $! vx * pp'
                  pure pp'
                pure (offset + lenSofar, pp')
            )
            (lenSofar, 1 :: Int)
          $ VU.generate np (+ 1)
    )
    (1 :: Int)
    pns
  pure vec
  where
    pns = primeFactors x
    (!_, !ns) = VU.unzip pns
    nDivisors = VU.foldl' (\ !acc n -> acc * (n + 1)) (1 :: Int) ns

-- | Returns a primitive root modulo \(p\), where \(p\) is a prime number.
--
-- ==== Constraints
-- - \(p\) must be a prime number.
--
-- ==== __Example__
-- >>> primitiveRoot 999999999999999989
-- 833278905416200545
--
-- >>> primitiveRoot 100055128505716009
-- 40765942246299710
--
-- @since 1.2.7.0
{-# INLINEABLE primitiveRoot #-}
primitiveRoot :: (HasCallStack) => Int -> Int
primitiveRoot x
  | not (isPrime x) = error $ "AtCoder.Extra.Math.primitiveRoot: give non-prime value `" ++ show x ++ "`"
  | x == 2 = 1
primitiveRoot x = tryRandom $ mkStdGen 123456789
  where
    !x64 :: Word64 = fromIntegral x
    !mont = M64.fromVal x64
    (!ps, !_) = VU.unzip $ primeFactorsUnsorted (x - 1)
    -- g s.t. for all p_i. g^n mod p_i /= 1
    test :: (HasCallStack) => Word64 -> Int -> Bool
    test g p =
      let !n = (x - 1) `div` p
       in (/= 1)
            . M64.decode mont
            . power (M64.mulMod mont) n
            . M64.encode mont
            $ fromIntegral g
    tryRandom :: (HasCallStack) => StdGen -> Int
    tryRandom !gen
      | VU.all (test rnd) ps = fromIntegral rnd
      | otherwise = tryRandom gen'
      where
        (!rnd, !gen') = uniformR (1, x64 - 1) gen

-- | Calculates \(f^n(x)\) with custom multiplication operator using the binary exponentiation
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
power :: (HasCallStack) => (a -> a -> a) -> Int -> a -> a
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

-- TODO: powMod for arbitrary modulus value (needs Barrett64)

-- -- | \(O(\log n)\) One-shot \(x^n \bmod m\) calculation.
-- --
-- -- @since 1.2.7.0
-- {-# INLINE powMod #-}
-- powMod :: (HasCallStack) => Int -> Int -> Int -> Int
-- powMod x n m =
--   fromIntegral
--     . M64.decode mont
--     . power (M64.mulMod mont) n
--     . M64.encode mont
--     $ fromIntegral x
--   where
--     !mont = M64.fromVal $! fromIntegral m

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
stimes' :: (HasCallStack) => (Semigroup a) => Int -> a -> a
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
mtimes' :: (HasCallStack) => (Monoid a) => Int -> a -> a
mtimes' n x = case compare n 0 of
  LT -> error "AtCoder.Extra.Math.mtimes': non-negative multiplier expected"
  EQ -> mempty
  GT -> power (<>) n x
