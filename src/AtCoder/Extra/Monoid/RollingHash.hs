{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | Rolling hash algorithm implemented as a monoid, typically stored in a segment tree. The type
-- parameters \(b\) and \(p\) represent the B-adic base and the modulus, respectively.
--
-- Combining `RollingHash` with `SegTree` enables \(O(\log |s|)\) string slice creation and
-- \(O(1)\) slice comparison.
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
import Data.WideWord.Word128
import GHC.Exts (proxy#)
import GHC.TypeNats (KnownNat, natVal')

-- | Rolling hash algorithm implemented as a monoid, typically stored in a segment tree. The type
-- parameters \(b\) and \(p\) represent the B-adic base and the modulus, respectively.
--
-- Combining `RollingHash` with `SegTree` enables \(O(\log |s|)\) string slice creation and
-- \(O(1)\) slice comparison.
--
-- ==== __Example__
-- It's convenient to define a type alias of `RollingHash`:
--
-- >>> import AtCoder.Extra.Monoid.RollingHash qualified as RH
-- >>> import AtCoder.SegTree qualified as ST
-- >>> import Data.Char (ord)
-- >>> import Data.Semigroup (Dual (..))
-- >>> type RH = RH.RollingHash 100 2305843009213693951
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
    --
    -- @since 1.1.0.0
    hashRH :: {-# UNPACK #-} !Int,
    -- | \(b^{\mathrm{length}} \bmod p\).
    --
    -- @since 1.1.0.0
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
new h = RollingHash (h `rem` fromIntegral (natVal' (proxy# @p))) (fromIntegral (natVal' (proxy# @b)))

-- | \(O(1)\) Creates a one-length `RollingHash` from an integer without taking the mod.
--
-- @since 1.1.0.0
{-# INLINE unsafeNew #-}
unsafeNew :: forall b p. (KnownNat b, KnownNat p) => Int -> RollingHash b p
unsafeNew h = RollingHash h (fromIntegral (natVal' (proxy# @b)))

-- | \(O(1)\)
{-# INLINE calc #-}
calc :: forall b p. (KnownNat b, KnownNat p) => RollingHash b p -> RollingHash b p -> RollingHash b p
calc (RollingHash !digit1 !hash1) (RollingHash !digit2 !hash2)
  | p < 3037000499 =
      let !digit' = digit1 * digit2 `rem` p
          !hash' = addMod p (hash1 * digit2 `rem` p) hash2
       in RollingHash digit' hash'
  | otherwise =
      -- TODO: This is slow
      let !digit' = fromIntegral $! to128 digit1 * to128 digit2 `rem` to128 p
          !hash' = fromIntegral $! (to128 hash1 * to128 digit2 + to128 hash2) `rem` to128 p
       in RollingHash digit' hash'
  where
    !p = fromIntegral $ natVal' (proxy# @p)
    to128 :: Int -> Word128
    to128 = fromIntegral
    addMod :: Int -> Int -> Int -> Int
    addMod m x y
      | x + y >= m = x + y - m
      | otherwise = x + y

-- | @since 1.1.0.0
instance (KnownNat b, KnownNat p) => Semigroup (RollingHash b p) where
  {-# INLINE (<>) #-}
  (<>) = calc

-- | @since 1.1.0.0
instance (KnownNat b, KnownNat p) => Monoid (RollingHash b p) where
  {-# INLINE mempty #-}
  mempty = RollingHash 1 0

type RHRepr = (Int, Int)

-- | @since 1.1.0.0
instance VU.IsoUnbox (RollingHash b p) RHRepr where
  {-# INLINE toURepr #-}
  toURepr (RollingHash a b) = (a, b)
  {-# INLINE fromURepr #-}
  fromURepr (!a, !b) = RollingHash a b

-- | @since 1.1.0.0
newtype instance VU.MVector s (RollingHash b p) = MV_RH (VUM.MVector s RHRepr)

-- | @since 1.1.0.0
newtype instance VU.Vector (RollingHash b p) = V_RH (VU.Vector RHRepr)

-- | @since 1.1.0.0
deriving via (RollingHash b p `VU.As` RHRepr) instance VGM.MVector VUM.MVector (RollingHash b p)

-- | @since 1.1.0.0
deriving via (RollingHash b p `VU.As` RHRepr) instance VG.Vector VU.Vector (RollingHash b p)

-- | @since 1.1.0.0
instance VU.Unbox (RollingHash b p)
