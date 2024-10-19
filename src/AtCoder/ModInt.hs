{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}

module AtCoder.ModInt
  ( ModInt998244353,
    ModInt1000000007,
    StaticModInt (..),
    modVal,
    modVal#,
    raw,
    new,
    val,
    pow,
    invPrime,
    invNonPrime,
  )
where

import AtCoder.Internal.Assert (runtimeAssert)
import AtCoder.Internal.Math qualified as ACIM
import Data.Bits
import Data.Coerce
import Data.Proxy (Proxy)
import Data.Ratio
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Primitive qualified as P
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed qualified as VU
import GHC.Exts (Proxy#, proxy#)
import GHC.TypeLits

-- TODO: faster internal implementation

type ModInt998244353 = StaticModInt 998244353;

type ModInt1000000007 = StaticModInt 1000000007;

-- type ModInt998244353 = DynamicModInt (-1);

newtype StaticModInt a = StaticModInt {unStaticModInt :: Int}
  deriving (P.Prim)
  deriving newtype (Eq, Ord, Read, Show)

-- | Retrieves the staticaly defined modulo value.
modVal :: (KnownNat a) => Proxy a -> Int
modVal = fromInteger . natVal

-- | Retrieves the staticaly defined modulo value.
modVal# :: forall a. (KnownNat a) => Proxy# a -> Int
modVal# p = fromInteger $ natVal' p

-- | \(O(1)\) Creates `StaticModInt` without taking the mod.
raw :: Int -> StaticModInt a
raw = StaticModInt

new :: forall a. (KnownNat a) => Int -> StaticModInt a
new v = StaticModInt $! v `mod` fromInteger (natVal' (proxy# @a))

-- | \(O(1)\)
val :: StaticModInt a -> Int
val = unStaticModInt

-- | \(O(W)\)
pow :: (KnownNat a) => StaticModInt a -> Int -> StaticModInt a
pow x0 n = inner 1 x0 n
  where
    !_ = runtimeAssert (n >= 0) $ "pow: `n` must not be negative: `" ++ show n ++ "`"
    inner !acc !dx !i
      | i == 0 = acc
      | otherwise =
          let !acc' = if testBit i 0 then acc * dx else acc
           in inner acc' (dx * dx) (i .>>. 1)

-- TODO: test prime at compile time

invPrime :: (KnownNat a) => StaticModInt a -> StaticModInt a
invPrime _ = undefined

invNonPrime :: (KnownNat a) => StaticModInt a -> StaticModInt a
invNonPrime _ = undefined

instance (KnownNat a) => Num (StaticModInt a) where
  (StaticModInt x1) + (StaticModInt x2) = StaticModInt $! (x1 + x2) `mod` modVal# (proxy# @a)
  (StaticModInt x1) * (StaticModInt x2) = StaticModInt $! (x1 * x2) `mod` modVal# (proxy# @a)
  negate (StaticModInt !v) = StaticModInt $! (-v) `mod` modVal# (proxy# @a)
  abs = id
  signum _ = 1
  fromInteger = new . fromInteger

instance (KnownNat a) => Integral (StaticModInt a) where
  quotRem x y = (x / y, x - x / y * y)
  toInteger = coerce (toInteger @Int)

instance (KnownNat a) => Fractional (StaticModInt a) where
  recip = invNonPrime
  fromRational !r = StaticModInt n / StaticModInt d
    where
      -- Data.Ratio
      n = fromInteger $! numerator r
      d = fromInteger $! denominator r

instance (KnownNat a) => Enum (StaticModInt a) where
  toEnum = StaticModInt . (`mod` fromInteger (natVal' (proxy# @a)))
  fromEnum = coerce

deriving newtype instance (KnownNat p) => Real (StaticModInt p)

newtype instance VU.MVector s (StaticModInt a) = MV_StaticModInt (VU.MVector s Int)

newtype instance VU.Vector (StaticModInt a) = V_StaticModInt (VU.Vector Int)

deriving newtype instance VGM.MVector VU.MVector (StaticModInt a)

deriving newtype instance VG.Vector VU.Vector (StaticModInt a)

instance VU.Unbox (StaticModInt a)
