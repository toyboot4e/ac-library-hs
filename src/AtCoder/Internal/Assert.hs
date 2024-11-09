-- | Runtime assertion utility.
module AtCoder.Internal.Assert
  ( runtimeAssert,
    checkIndex,
    checkVertex,
    checkEdge,
    checkCustom,
    checkInterval,
  )
where

import GHC.Stack (HasCallStack)

-- | Assertion that is never erased at compile time.
{-# INLINE runtimeAssert #-}
runtimeAssert :: (HasCallStack) => Bool -> String -> ()
runtimeAssert p s
  | p = ()
  | otherwise = error s

-- TODO: see also vector.

{-# INLINE checkIndex #-}
checkIndex :: String -> Int -> Int -> ()
checkIndex funcName i n
  | 0 <= i && i < n = ()
  | otherwise = error $ funcName ++ ": given invalid index`" ++ show i ++ "` over length `" ++ show n ++ "`"

{-# INLINE checkVertex #-}
checkVertex :: String -> Int -> Int -> ()
checkVertex funcName i n
  | 0 <= i && i < n = ()
  | otherwise = error $ funcName ++ ": given invalid vertex`" ++ show i ++ "` over the number of vertices `" ++ show n ++ "`"

{-# INLINE checkEdge #-}
checkEdge :: String -> Int -> Int -> ()
checkEdge funcName i n
  | 0 <= i && i < n = ()
  | otherwise = error $ funcName ++ ": given invalid edge index`" ++ show i ++ "` over the number of edges `" ++ show n ++ "`"

{-# INLINE checkCustom #-}
checkCustom :: String -> String -> Int -> String -> Int -> ()
checkCustom funcName indexName i setName n
  | 0 <= i && i < n = ()
  | otherwise = error $ funcName ++ ": given invalid " ++ indexName ++ " `" ++ show i ++ "` over " ++ setName ++ " `" ++ show n ++ "`"

-- | Checks a half-open intervanl @[l, r)@ over intervanl @[0, n)@.
{-# INLINE checkInterval #-}
checkInterval :: String -> Int -> Int -> Int -> ()
checkInterval funcName l r n
  | 0 <= l && l <= r && r <= n = ()
  | otherwise = error $ funcName ++ ": given invalid interval `[" ++ show l ++ ", " ++ show r ++ ")` over length `" ++ show n ++ "`"
