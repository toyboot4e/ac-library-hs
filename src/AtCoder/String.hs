-- | It contains string algorithms.
--
-- Let @s@ be a string. We denote the substring of @s@ between \(a\)-th and \(b - 1\)-th character
-- by @s[a..b)@.
--
-- ==== Examples
--
-- ===== Suffix Array and LCP Array
--
-- >>> import AtCoder.String qualified as S
-- >>> import Data.ByteString.Char8 qualified as BS
-- >>> let s = BS.pack "aab"
-- >>> let sa = S.suffixArrayBS s
-- >>> S.lcpArrayBS s sa
-- [1,0]
--
-- ===== Z Algorithm
--
-- >>> import AtCoder.String qualified as S
-- >>> import Data.ByteString.Char8 qualified as BS
-- >>> let s = BS.pack "abab"
-- >>> S.zAlgorithmBS s
-- [4,0,2,0]
--
-- @since 1.0.0
module AtCoder.String
  ( -- * Suffix array
    suffixArray,
    suffixArrayBS,
    suffixArrayOrd,
    -- * LCP array
    lcpArray,
    lcpArrayBS,
    zAlgorithm,
    -- * Z algorithm
    zAlgorithmBS,
  )
where

import AtCoder.Internal.Assert qualified as ACIA
import AtCoder.Internal.String qualified as ACIS
import Control.Monad.ST (runST)
import Data.ByteString.Char8 qualified as BS
import Data.Char (ord)
import Data.Vector.Algorithms.Intro qualified as VAI
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Stack (HasCallStack)

-- TODO: document `chr`

-- | Calculates suffix array for a `Int` vector.
--
-- Given a string @s@ of length \(n\), it returns the suffix array of @s@. Here, the suffix array
-- @sa@ of @s@ is a permutation of \(0, \cdots, n-1\) such that @s[sa[i]..n) < s[sa[i+1]..n)@ holds
-- for all \(i = 0,1, \cdots ,n-2\).
--
-- ==== Constraints
-- - \(0 \leq n\)
-- - \(0 \leq \mathrm{upper} \leq 10^8\)
-- - \(0 \leq x \leq \mathrm{upper}\) for all elements \(x\) of \(s\).
--
-- ==== Complexity
-- - (3) \(O(n + \mathrm{upper})\)-time
--
-- @since 1.0.0
{-# INLINE suffixArray #-}
suffixArray :: (HasCallStack) => VU.Vector Int -> Int -> VU.Vector Int
suffixArray s upper =
  let !_ = ACIA.runtimeAssert (0 <= upper) $ "AtCoder.String.suffixArray: given negative `upper`: " ++ show upper
      -- FIXME: does it work?
      !_ = flip VU.map s $ \d -> do
        let !_ = ACIA.runtimeAssert (0 <= d && d <= upper) "AtCoder.String.suffixArray: "
         in ()
   in ACIS.saIs s upper

-- | Calculates suffix array for a @ByteString@.
--
-- ==== Constraints
-- - \(0 \leq n\)
--
-- ==== Complexity
-- - (1) \(O(n)\)-time
--
-- @since 1.0.0
{-# INLINE suffixArrayBS #-}
suffixArrayBS :: (HasCallStack) => BS.ByteString -> VU.Vector Int
suffixArrayBS s = do
  let n = BS.length s
      -- FIXME: correct? (upper 255?)
      s2 = VU.map ord $ VU.fromListN n (BS.unpack s)
   in ACIS.saIs s2 255

-- | Calculates suffix array for a `Ord` type vector.
--
-- ==== Constraints
-- - \(0 \leq n\)
--
-- ==== Complexity
-- - (2) \(O(n \log n)\)-time, \(O(n)\)-space
--
-- @since 1.0.0
{-# INLINE suffixArrayOrd #-}
suffixArrayOrd :: (HasCallStack, Ord a, VU.Unbox a) => VU.Vector a -> VU.Vector Int
suffixArrayOrd s =
  let n = VU.length s
      (!upper, !s2) = runST $ do
        let f i j = compare (s VG.! i) (s VG.! j)
        -- modify + generate should fuse
        let idx = VU.modify (VAI.sortBy f) $ VU.generate n id
        vec <- VUM.unsafeNew n
        upper_ <-
          VU.foldM'
            ( \now i -> do
                let now' =
                      if i > 0 && s VG.! (idx VG.! (i - 1)) /= s VG.! (idx VG.! i)
                        then now + 1
                        else now
                VGM.write vec (idx VG.! i) now'
                pure now'
            )
            (0 :: Int)
            (VU.generate n id)
        (upper_,) <$> VU.unsafeFreeze vec
   in ACIS.saIs s2 upper

-- | Given a string @s@ of length \(n\), it returns the LCP array of @s@. Here, the LCP array of
-- @s@ is the array of length \(n-1\), such that the \(i\)-th element is the length of the LCP
-- (Longest Common Prefix) of @s[sa[i]..n)@ and @s[sa[i+1]..n)@
--
-- ==== Constraints
-- - The second argument is the suffix array of @s@.
-- - \(1 \leq n\)
--
-- ==== Complexity
-- - \(O(n)\)
--
-- @since 1.0.0
{-# INLINE lcpArray #-}
lcpArray :: (HasCallStack, Ord a, VU.Unbox a) => VU.Vector a -> VU.Vector Int -> VU.Vector Int
lcpArray s sa =
  let n = VU.length s
      !_ = ACIA.runtimeAssert (n >= 1) "AtCoder.String.lcpArray: given empty input"
      rnk = VU.create $ do
        rnkVec <- VUM.unsafeNew @_ @Int n
        VU.iforM_ sa $ \i saI -> do
          VGM.write rnkVec saI i
        pure rnkVec
   in VU.create $ do
        lcp <- VUM.unsafeNew (n - 1)
        VU.ifoldM'_
          ( \ !h_ i rnkI -> do
              let h = if h_ > 0 then h_ - 1 else h_
              if rnkI == 0
                then pure h
                else do
                  let j = sa VG.! (rnkI - 1)
                  let inner !h'
                        | not $ j + h' < n && i + h' < n = h'
                        | s VG.! (j + h') /= s VG.! (i + h') = h'
                        | otherwise = inner $ h' + 1
                  let !h' = inner h
                  VGM.write lcp (rnkI - 1) h'
                  pure h'
          )
          (0 :: Int)
          rnk
        pure lcp

-- | @ByteString@ verison of `lcpArray`.
--
-- ==== Constraints
-- - The second argument is the suffix array of @s@.
-- - \(1 \leq n\)
--
-- ==== Complexity
-- - \(O(n)\)
--
-- @since 1.0.0
{-# INLINE lcpArrayBS #-}
lcpArrayBS :: (HasCallStack) => BS.ByteString -> VU.Vector Int -> VU.Vector Int
lcpArrayBS s sa =
  let n = BS.length s
      s2 = VU.map ord . VU.fromListN n $ BS.unpack s
   in lcpArray s2 sa

-- | Given a `Ord` vector of length \(n\), it returns the array of length \(n\), such that the
-- \(i\)-th element is the length of the LCP (Longest Common Prefix) of @s[0..n)@ and @s[i..n)@.
--
-- ==== Constraints
-- - \(n \leq n\)
-- ==== Complexity
-- - \(O(n)\)
--
-- @since 1.0.0
{-# INLINE zAlgorithm #-}
zAlgorithm :: (Ord a, VU.Unbox a) => VU.Vector a -> VU.Vector Int
zAlgorithm s
  | n == 0 = VU.empty
  | otherwise = VU.create $ do
      z <- VUM.unsafeNew @_ @Int n
      VGM.write z 0 0
      VU.foldM'_
        ( \j i -> do
            zj <- VGM.read z j
            k0 <-
              if j + zj <= i
                then pure 0
                else do
                  zij <- VGM.read z (i - j)
                  pure $ min (j + zj - i) zij
            let loop k
                  | i + k < n && s VG.! k == s VG.! (i + k) = loop (k + 1)
                  | otherwise = k
            let k = loop k0
            VGM.write z i k
            pure $
              if j + zj < i + k
                then i
                else j
        )
        (0 :: Int)
        (VU.generate (n - 1) (+ 1))
      VGM.write z 0 n
      pure z
  where
    n = VU.length s

-- | Given a string of length \(n\), it returns the array of length \(n\), such that the \(i\)-th
-- element is the length of the LCP (Longest Common Prefix) of @s[0..n)@ and @s[i..n)@.
--
-- ==== Constraints
-- - \(n \leq n\)
-- ==== Complexity
-- - \(O(n)\)
--
-- @since 1.0.0
{-# INLINE zAlgorithmBS #-}
zAlgorithmBS :: BS.ByteString -> VU.Vector Int
zAlgorithmBS s = zAlgorithm $ VU.fromListN (BS.length s) (BS.unpack s)
