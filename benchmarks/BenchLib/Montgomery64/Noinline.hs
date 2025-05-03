{-# LANGUAGE MagicHash #-}

module BenchLib.Montgomery64.Noinline (Montgomery64 (..), new, fromVal) where

import AtCoder.Internal.Assert qualified as ACIA
import Data.Bits (bit, (!>>.))
import Data.WideWord.Word128 (Word128 (..))
import Data.Word (Word64)
import GHC.Exts (Proxy#)
import GHC.Stack (HasCallStack)
import GHC.TypeNats (KnownNat, natVal')

data Montgomery64 = Montgomery64
  { mM64 :: {-# UNPACK #-} !Word64,
    rM64 :: {-# UNPACK #-} !Word64,
    n2M64 :: {-# UNPACK #-} !Word64
  }
  deriving
    ( Eq,
      Show
    )

{-# INLINE new #-}
new :: forall a. (HasCallStack, KnownNat a) => Proxy# a -> Montgomery64
new p = fromVal . fromIntegral $! natVal' p

{-# NOINLINE fromVal #-}
fromVal :: (HasCallStack) => Word64 -> Montgomery64
fromVal m =
  let !m128 :: Word128 = fromIntegral m
      !n2 = word128Lo64 $ (-m128) `mod` m128
      !r = getR m 0
      !_ = ACIA.runtimeAssert (r * m == -1) "AtCoder.Extra.Montgomery64.fromVal: internal implementation error"
   in Montgomery64 m r n2
  where
    !_ = ACIA.runtimeAssert (odd m && m <= bit 62) $ "AtCoder.Extra.Montgomery64.fromVal: not given odd modulus value that is less than or equal to 2^62: " ++ show m
    getR :: Word64 -> Int -> Word64
    getR !acc i
      | i >= 5 = -acc
      | otherwise = getR (acc * (2 - m * acc)) (i + 1)
