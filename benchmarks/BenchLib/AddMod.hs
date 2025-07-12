{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}

module BenchLib.AddMod
  ( addModMod,
    addModRem,
    addModSub,
    addModRem#,
    addModSub#,
    -- addMod3,
    -- addMod4,
    -- addMod5,
  )
where

import Data.Word (Word32, Word64)
import GHC.Exts
-- import GHC.Prim
import GHC.Word

{-# INLINE as64 #-}
as64 :: Word32 -> Word64
as64 = fromIntegral

-- TODO: try sub

{-# INLINE addModMod #-}
addModMod :: Word32 -> Word32 -> Word32 -> Word32
addModMod m a b = (a + b) `mod` m

{-# INLINE addModRem #-}
addModRem :: Word32 -> Word32 -> Word32 -> Word32
addModRem m a b = (a + b) `rem` m

{-# INLINE addModSub #-}
addModSub :: Word32 -> Word32 -> Word32 -> Word32
addModSub m a b
  | x >= m = x - m
  | otherwise = x
  where
    x = a + b

-- taken from iota
-- TODO: try phases

{-# INLINE addModRem# #-}
addModRem# :: Word32 -> Word32 -> Word32 -> Word32
addModRem# (W32# m#) (W32# x#) (W32# y#) = W32# ((x# `plusWord32#` y#) `remWord32#` m#)

{-# INLINE addModSub# #-}
addModSub# :: Word32 -> Word32 -> Word32 -> Word32
addModSub# (W32# m#) (W32# x#) (W32# y#) = case x# `plusWord32#` y# of
  r#
    | isTrue# (r# `leWord32#` m#) -> W32# r#
    | otherwise -> W32# (r# `subWord32#` m#)

-- FIXME: understand the code and translate

-- {-# INLINE addMod3 #-}
-- addMod3 :: Word32 -> Word32 -> Word32 -> Word32
-- addMod3 (W32# m#) (W32# x#) (W32# y#) = case x# `plusWord32#` y# of
--   r# -> W32# (r# `subWord32#` ((r# `geWord32#` m#) `timesWord32#` m#))
--
-- {-# INLINE addMod4 #-}
-- addMod4 :: Word32 -> Word32 -> Word32 -> Word32
-- addMod4 (W32# m#) (W32# x#) (W32# y#) = case x# plusWord32# y# of
--   r# -> W32# (r# -# (m# *# (r# >=# m#)))
--
-- {-# INLINE addMod5 #-}
-- addMod5 :: Word32 -> Word32 -> Word32 -> Word32
-- addMod5 (W32# m#) (W32# x#) (W32# y#) = W32# (x# plusWord32# y# -# (m# *# (x# plusWord32# y# >=# m#)))
