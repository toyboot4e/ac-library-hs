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

-- | Asserts \(0 \leq i \lt n\) for an array index \(i\).
{-# INLINE checkIndex #-}
checkIndex :: (HasCallStack) =>String -> Int -> Int -> ()
checkIndex funcName i n
  | 0 <= i && i < n = ()
  | otherwise = error $ funcName ++ ": given invalid index`" ++ show i ++ "` over length `" ++ show n ++ "`"

-- | Asserts \(0 \leq i \lt n\) for a graph vertex \(i\).
{-# INLINE checkVertex #-}
checkVertex :: (HasCallStack) =>String -> Int -> Int -> ()
checkVertex funcName i n
  | 0 <= i && i < n = ()
  | otherwise = error $ funcName ++ ": given invalid vertex`" ++ show i ++ "` over the number of vertices `" ++ show n ++ "`"

-- | Asserts \(0 \leq i \lt m\) for an edge index \(i\).
{-# INLINE checkEdge #-}
checkEdge :: (HasCallStack) =>String -> Int -> Int -> ()
checkEdge funcName i n
  | 0 <= i && i < n = ()
  | otherwise = error $ funcName ++ ": given invalid edge index`" ++ show i ++ "` over the number of edges `" ++ show n ++ "`"

-- | Asserts \(0 \leq i \lt n\) with custom index/set names.
{-# INLINE checkCustom #-}
checkCustom :: (HasCallStack) =>String -> String -> Int -> String -> Int -> ()
checkCustom funcName indexName i setName n
  | 0 <= i && i < n = ()
  | otherwise = error $ funcName ++ ": given invalid " ++ indexName ++ " `" ++ show i ++ "` over " ++ setName ++ " `" ++ show n ++ "`"

-- | Asserts \(0 \leq l \leq r \leq n\) for a half-open interval \([l, r)\).
{-# INLINE checkInterval #-}
checkInterval :: (HasCallStack) =>String -> Int -> Int -> Int -> ()
checkInterval funcName l r n
  | 0 <= l && l <= r && r <= n = ()
  | otherwise = error $ funcName ++ ": given invalid interval `[" ++ show l ++ ", " ++ show r ++ ")` over length `" ++ show n ++ "`"
