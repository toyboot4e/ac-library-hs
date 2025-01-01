{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | The rolling hash algorithm implemented as a monoid, typically stored in a segment tree. The
-- type parameter @b@ represents the B-adic number and @p@ represents the modulus value.
--
-- @since 1.1.0.0
module AtCoder.Extra.Monoid.RollingHash
  ( -- * Rolling hash
    RollingHash (..),

    -- * Constructors
    new,
    unsafeNew,
  )
where

import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Exts (proxy#)
import GHC.TypeNats (KnownNat, natVal')

-- | The rolling hash algorithm implemented as a monoid, typically stored in a segment tree. The
-- type parameter @b@ represents the B-adic number and @p@ represents the modulus value.
--
-- ==== __Example__
-- It's convenient to define a type alias of `RollingHash`:
--
-- >>> import AtCoder.Extra.Monoid.RollingHash qualified as RH
-- >>> import AtCoder.SegTree qualified as ST
-- >>> import Data.Char (ord)
-- >>> import Data.Semigroup (Dual (..))
-- >>> type RH = RH.RollingHash 100 998244353
--
-- Let's test whether "abcba" is a palindrome:
--
-- >>> seg <- ST.build @_ @RH . VU.map (RH.unsafeNew . ord) $ VU.fromList "abcba"
-- >>> seg' <- ST.build @_ @(Dual RH) . VU.map (Dual . RH.unsafeNew . ord) $ VU.fromList "abcba"
-- >>> hash1 <- ST.prod seg 2 5       --   cba  (left to right)
-- >>> Dual hash2 <- ST.prod seg' 0 3 -- abc    (right to lett)
-- >>> hash1 == hash2
-- True
--
-- @since 1.1.0.0
data RollingHash b p = RollingHash
  { -- | The hash value.
    hashRH :: {-# UNPACK #-} !Int,
    -- | \(b^{\mathrm{length}} \bmod p\).
    nextDigitRH :: {-# UNPACK #-} !Int
  }
  deriving
    ( -- | @since 1.1.0.0
      Eq,
      -- | @since 1.1.0.0
      Show
    )

-- | \(O(1)\) Creates a one-length `RollingHash` from an integer.
--
-- @since 1.1.0.0
{-# INLINE new #-}
new :: forall b p. (KnownNat b, KnownNat p) => Int -> RollingHash b p
new h = RollingHash (h `mod` fromIntegral (natVal' (proxy# @p))) (fromIntegral (natVal' (proxy# @b)))

-- | \(O(1)\) Creates a one-length `RollingHash` from an integer without taking the mod.
--
-- @since 1.1.0.0
{-# INLINE unsafeNew #-}
unsafeNew :: forall b p. (KnownNat b, KnownNat p) => Int -> RollingHash b p
unsafeNew h = RollingHash h (fromIntegral (natVal' (proxy# @b)))

-- | @since 1.1.0.0
instance (KnownNat b, KnownNat p) => Semigroup (RollingHash b p) where
  -- \| \(O(1)\)
  {-# INLINE (<>) #-}
  (RollingHash !digit1 !hash1) <> (RollingHash !digit2 !hash2) = RollingHash digit' hash'
    where
      !p = fromIntegral $ natVal' (proxy# @p)
      !digit' = digit1 * digit2 `mod` p
      !hash' = (hash1 * digit2 + hash2) `mod` p

-- | @since 1.1.0.0
instance (KnownNat b, KnownNat p) => Monoid (RollingHash b p) where
  {-# INLINE mempty #-}
  mempty = RollingHash 1 0

type RHRepr = (Int, Int)

instance VU.IsoUnbox (RollingHash b p) RHRepr where
  {-# INLINE toURepr #-}
  toURepr (RollingHash a b) = (a, b)
  {-# INLINE fromURepr #-}
  fromURepr (!a, !b) = RollingHash a b

newtype instance VU.MVector s (RollingHash b p) = MV_RH (VUM.MVector s RHRepr)

newtype instance VU.Vector (RollingHash b p) = V_RH (VU.Vector RHRepr)

deriving via (RollingHash b p `VU.As` RHRepr) instance VGM.MVector VUM.MVector (RollingHash b p)

deriving via (RollingHash b p `VU.As` RHRepr) instance VG.Vector VU.Vector (RollingHash b p)

instance VU.Unbox (RollingHash b p)
