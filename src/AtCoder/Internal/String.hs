-- | Internal implementation of @AtCoder.String@ module.
module AtCoder.Internal.String
  ( -- * Suffix array
    saNaive,
    saDoubling,
    saIsImpl,
    saIs,
    saIsManual,
  )
where

import Control.Monad (unless, when)
import Control.Monad.ST (runST)
import Data.Bit (Bit (..))
import Data.Foldable (for_)
import Data.Vector.Algorithms.Intro qualified as VAI
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Stack (HasCallStack)

-- TODO: remove `HasCallStack`?

-- | \(O(n^2)\) Internal implementation of suffix array creation (naive).
--
-- @since 1.0.0.0
{-# INLINEABLE saNaive #-}
saNaive :: (HasCallStack) => VU.Vector Int -> VU.Vector Int
saNaive s =
  let n = VU.length s
      f l0 r0
        | l0 == r0 = GT
        | otherwise = inner l0 r0
        where
          inner l r
            | l < n && r < n =
                let sl = s VG.! l
                    sr = s VG.! r
                 in if sl /= sr
                      then compare sl sr
                      else inner (l + 1) (r + 1)
            | l == n = LT
            | otherwise = GT
   in -- modify + generate should fuse
      VU.modify (VAI.sortBy f) $ VU.generate n id

-- | \(O(n \log n)\) Internal implementation of suffix array creation (doubling).
--
-- @since 1.0.0.0
{-# INLINEABLE saDoubling #-}
saDoubling :: (HasCallStack) => VU.Vector Int -> VU.Vector Int
saDoubling s = VU.create $ do
  let n = VU.length s
  sa <- VUM.generate n id
  let loop rnk tmp k
        | k >= n = pure ()
        | otherwise = do
            rnk' <- VU.unsafeFreeze rnk
            let cmp x y =
                  let rnkX = rnk' VG.! x
                      rnkY = rnk' VG.! y
                   in if rnkX /= rnkY
                        then compare rnkX rnkY
                        else
                          let rx = if x + k < n then rnk' VG.! (x + k) else (-1)
                              ry = if y + k < n then rnk' VG.! (y + k) else (-1)
                           in compare rx ry
            VAI.sortBy cmp sa
            sa' <- VU.unsafeFreeze sa
            VGM.write tmp (sa' VG.! 0) 0
            VU.zipWithM_
              ( \saI_ saI -> do
                  tmpI_ <- VGM.read tmp saI_
                  VGM.write tmp saI $! tmpI_ + if cmp saI_ saI == LT then 1 else 0
              )
              sa'
              (VU.tail sa')
            loop tmp rnk (2 * k)
  rnkVec <- VU.thaw s
  tmpVec <- VUM.unsafeNew n
  loop rnkVec tmpVec 1
  pure sa

-- TODO: use backpermute

-- | \(O(n)\) Internal implementation of suffix array creation (suffix array induced sorting).
--
-- @since 1.0.0.0
{-# INLINEABLE saIsImpl #-}
saIsImpl ::
  (HasCallStack) =>
  -- | naive threshould
  Int ->
  -- | doubling threshould
  Int ->
  -- | string
  VU.Vector Int ->
  -- | upper bounds
  Int ->
  -- | suffix array
  VU.Vector Int
saIsImpl naiveThreshold doublingThreshold s upper = VU.create $ do
  let n = VU.length s
  let !ls = VU.create $ do
        ls_ <- VUM.unsafeNew @_ @Bit n
        VGM.write ls_ (n - 1) $ Bit False
        for_ [n - 2, n - 3 .. 0] $ \i -> do
          let si = s VG.! i
          let si1 = s VG.! (i + 1)
          b <-
            if si == si1
              then VGM.read ls_ (i + 1)
              else pure . Bit $ si < si1
          VGM.write ls_ i b
        pure ls_

  let (!sumL, !sumS) = runST $ do
        sumL_ <- VUM.replicate (upper + 1) (0 :: Int)
        sumS_ <- VUM.replicate (upper + 1) (0 :: Int)
        VU.zipWithM_
          ( \(Bit !b) !si -> do
              if not b
                then VGM.modify sumS_ (+ 1) si
                else VGM.modify sumL_ (+ 1) $ si + 1
          )
          ls
          s
        for_ [0 .. upper] $ \i -> do
          l <- VGM.read sumL_ i
          VGM.modify sumS_ (+ l) i
          when (i < upper) $ do
            si <- VGM.read sumS_ i
            VGM.modify sumL_ (+ si) $ i + 1
        sumL' <- VU.unsafeFreeze sumL_
        sumS' <- VU.unsafeFreeze sumS_
        pure (sumL', sumS')

  sa <- VUM.unsafeNew @_ @Int n
  let induce lms = do
        VGM.set sa (-1)
        buf <- VU.thaw sumS
        VU.forM_ lms $ \d -> do
          unless (d == n) $ do
            i <- VGM.read buf $ s VG.! d
            VGM.write buf (s VG.! d) $ i + 1
            VGM.write sa i d
        VU.iforM_ sumL (VGM.write buf)
        do
          i <- VGM.read buf $ s VG.! (n - 1)
          VGM.write buf (s VG.! (n - 1)) $ i + 1
          VGM.write sa i $ n - 1
        do
          -- TODO: try VUM.forM_
          for_ [0 .. n - 1] $ \i -> do
            v <- VGM.read sa i
            when (v >= 1 && not (unBit (ls VG.! (v - 1)))) $ do
              j <- VGM.read buf $ s VG.! (v - 1)
              VGM.write buf (s VG.! (v - 1)) $ j + 1
              VGM.write sa j $ v - 1
        VU.iforM_ sumL (VGM.write buf)
        -- TODO: try foldr
        for_ [n - 1, n - 2 .. 0] $ \i -> do
          v <- VGM.read sa i
          when (v >= 1 && unBit (ls VG.! (v - 1))) $ do
            j <- subtract 1 <$> VGM.read buf (s VG.! (v - 1) + 1)
            VGM.write buf (s VG.! (v - 1) + 1) j
            VGM.write sa j $ v - 1

  let (!lmsMap, !m) = runST $ do
        lmsMap_ <- VUM.replicate (n + 1) (-1 :: Int)
        len <-
          VU.ifoldM'
            ( \iAcc i (Bit !b1, Bit !b2) -> do
                if not b1 && b2
                  then do
                    VGM.write lmsMap_ (i + 1) iAcc
                    pure $ iAcc + 1
                  else do
                    pure iAcc
            )
            (0 :: Int)
            $ VU.zip ls (VU.tail ls)
        (,len) <$> VU.unsafeFreeze lmsMap_

  let lms = VU.create $ do
        lms_ <- VUM.unsafeNew @_ @Int m
        VU.ifoldM'_
          ( \iAcc i (Bit !b1, Bit !b2) -> do
              if not b1 && b2
                then do
                  VGM.write lms_ iAcc $ i + 1
                  pure $ iAcc + 1
                else do
                  pure iAcc
          )
          (0 :: Int)
          $ VU.zip ls (VU.tail ls)
        pure lms_

  induce lms

  when (m > 0) $ do
    sa' <- VU.unsafeFreeze sa
    sortedLms <- VUM.unsafeNew @_ @Int m
    VU.foldM'_
      ( \iAcc v -> do
          if lmsMap VG.! v /= -1
            then do
              VGM.write sortedLms iAcc v
              pure $ iAcc + 1
            else do
              pure iAcc
      )
      (0 :: Int)
      sa'
    recS <- VUM.unsafeNew @_ @Int m
    (\i -> VGM.write recS i 0) . (lmsMap VG.!) =<< VGM.read sortedLms 0

    recUpper' <- do
      sortedLms' <- VU.unsafeFreeze sortedLms
      VU.foldM'
        ( \recUpper (!l, !r) -> do
            let lmsMapL = lmsMap VG.! l
            let lmsMapR = lmsMap VG.! r
            let endL = if lmsMapL + 1 < m then lms VG.! (lmsMapL + 1) else n
            let endR = if lmsMapR + 1 < m then lms VG.! (lmsMapR + 1) else n
            let same
                  | endL - l /= endR - r = False
                  | otherwise = inner l r
                  where
                    inner x y
                      | x < endL && s VG.! x == s VG.! y = inner (x + 1) (y + 1)
                      | x == n || s VG.! x /= s VG.! y = False
                      | otherwise = True
            let recUpper' = if same then recUpper else recUpper + 1
            VGM.write recS lmsMapR recUpper'
            pure recUpper'
        )
        (0 :: Int)
        $ VU.zip sortedLms' (VU.tail sortedLms')

    recS' <- VU.unsafeFreeze recS
    let recSa = saIsManual naiveThreshold doublingThreshold recS' recUpper'

    VU.iforM_ recSa $ \i x -> do
      VGM.write sortedLms i (lms VG.! x)
    induce =<< VU.unsafeFreeze sortedLms

  pure sa

-- | \(O(n)\) Internal implementation of suffix array creation (suffix array induced sorting).
--
-- SA-IS, linear-time suffix array construction.
-- Reference:
-- G. Nong, S. Zhang, and W. H. Chan,
-- Two Efficient Algorithms for Linear Time Suffix Array Construction
--
-- @since 1.0.0.0
{-# INLINE saIs #-}
saIs ::
  (HasCallStack) =>
  -- | string
  VU.Vector Int ->
  -- | upper bounds
  Int ->
  -- | suffix array
  VU.Vector Int
saIs = saIsManual 10 40

-- | \(O(n)\) Internal implementation of suffix array creation (suffix array induced sorting).
--
-- SA-IS, linear-time suffix array construction.
-- Reference:
-- G. Nong, S. Zhang, and W. H. Chan,
-- Two Efficient Algorithms for Linear Time Suffix Array Construction
--
-- @since 1.0.0.0
{-# INLINE saIsManual #-}
saIsManual ::
  (HasCallStack) =>
  -- | naive threshold
  Int ->
  -- | doubling threshold
  Int ->
  -- | string
  VU.Vector Int ->
  -- | upper bounds
  Int ->
  -- | suffix array
  VU.Vector Int
saIsManual naiveThreshold doublingThreshold s upper
  | n == 0 = VU.empty
  | n == 1 = VU.singleton 0
  | n == 2 && s VG.! 0 < s VG.! 1 = VU.fromListN 2 [0, 1]
  | n == 2 = VU.fromListN 2 [1, 0]
  | n < naiveThreshold = saNaive s
  | n < doublingThreshold = saDoubling s
  | otherwise = saIsImpl naiveThreshold doublingThreshold s upper
  where
    n = VU.length s
