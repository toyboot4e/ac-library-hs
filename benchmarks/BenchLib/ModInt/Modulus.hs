{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}

-- | TypeNats-based.
module BenchLib.ModInt.Modulus
  ( Modulus (..),
  )
where

import AtCoder.Internal.Assert qualified as ACIA
import AtCoder.Internal.Math qualified as ACIM
import BenchLib.MulMod.BarrettWideWord qualified as BarrettWideWord
import Data.Bits
import Data.Coerce (coerce)
import Data.Tagged (Tagged(..))
import Data.Proxy (Proxy)
import Data.Ratio (denominator, numerator)
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Primitive qualified as P
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed qualified as VU
import Data.Word (Word32, Word64)
import GHC.Exts (Proxy#, proxy#)
import GHC.Stack (HasCallStack)
import GHC.TypeNats (KnownNat, natVal, natVal')

-- TODO: how to make a benchmark for this. maybe (^) opertor?

-- | `KnownNat` with meta information used for modulus.
class (KnownNat a) => Modulus a where
  intModulus :: Tagged a Int
  word32Modulus :: Tagged a Word32
  word64Modulus :: Tagged a Word64
  amb32Modulus :: Word32
  amb64Modulus :: Word64
  isPrimeModulus :: Proxy# a -> Bool

instance Modulus 998244353 where
  intModulus = Tagged 998244353
  word32Modulus = Tagged 998244353
  word64Modulus = Tagged 998244353
  amb32Modulus = 998244353
  amb64Modulus = 998244353
  isPrimeModulus _ = True
