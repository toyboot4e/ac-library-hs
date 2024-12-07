-- | Runtime assertion utility.
--
-- = Example
-- >>> let !_ = runtimeAssert False "errorMessage"
-- *** Exception: errorMessage
-- ...
-- >>> let !_ = checkIndex "AtCoder.Internal.Assert.test" 0 3
-- >>> let !_ = checkIndex "AtCoder.Internal.Assert.test" (-1) 3
-- *** Exception: AtCoder.Internal.Assert.test: given invalid index `-1` over length `3`
-- ...
-- >>> let !_ = checkVertex "AtCoder.Internal.Assert.test" 0 3
-- >>> let !_ = checkVertex "AtCoder.Internal.Assert.test" (-1) 3
-- *** Exception: AtCoder.Internal.Assert.test: given invalid vertex `-1` over the number of vertices `3`
-- ...
-- >>> let !_ = checkEdge "AtCoder.Internal.Assert.test" 0 3
-- >>> let !_ = checkEdge "AtCoder.Internal.Assert.test" (-1) 3
-- *** Exception: AtCoder.Internal.Assert.test: given invalid edge index `-1` over the number of edges `3`
-- ...
-- >>> let !_ = checkCustom "AtCoder.Internal.Assert.test" "index" 0 "set" 3
-- >>> let !_ = checkCustom "AtCoder.Internal.Assert.test" "index" (-1) "set" 3
-- *** Exception: AtCoder.Internal.Assert.test: given invalid index `-1` over set `3`
-- ...
-- >>> let !_ = checkInterval "AtCoder.Internal.Assert.test" 0 3 3
-- >>> let !_ = checkInterval "AtCoder.Internal.Assert.test" 0 4 3
-- *** Exception: AtCoder.Internal.Assert.test: given invalid interval `[0, 4)` over length `3`
-- ...
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

-- | \(O(1)\) Assertion that is never erased at compile time.
{-# INLINE runtimeAssert #-}
runtimeAssert :: (HasCallStack) => Bool -> String -> ()
runtimeAssert p s
  | p = ()
  | otherwise = error s

-- | \(O(1)\) Asserts \(0 \leq i \lt n\) for an array index \(i\).
{-# INLINE checkIndex #-}
checkIndex :: (HasCallStack) => String -> Int -> Int -> ()
checkIndex funcName i n
  | 0 <= i && i < n = ()
  | otherwise = error $ funcName ++ ": given invalid index `" ++ show i ++ "` over length `" ++ show n ++ "`"

-- | \(O(1)\) Asserts \(0 \leq i \lt n\) for a graph vertex \(i\).
{-# INLINE checkVertex #-}
checkVertex :: (HasCallStack) => String -> Int -> Int -> ()
checkVertex funcName i n
  | 0 <= i && i < n = ()
  | otherwise = error $ funcName ++ ": given invalid vertex `" ++ show i ++ "` over the number of vertices `" ++ show n ++ "`"

-- | \(O(1)\) Asserts \(0 \leq i \lt m\) for an edge index \(i\).
{-# INLINE checkEdge #-}
checkEdge :: (HasCallStack) => String -> Int -> Int -> ()
checkEdge funcName i n
  | 0 <= i && i < n = ()
  | otherwise = error $ funcName ++ ": given invalid edge index `" ++ show i ++ "` over the number of edges `" ++ show n ++ "`"

-- | \(O(1)\) Asserts \(0 \leq i \lt n\) with custom index/set names.
{-# INLINE checkCustom #-}
checkCustom :: (HasCallStack) => String -> String -> Int -> String -> Int -> ()
checkCustom funcName indexName i setName n
  | 0 <= i && i < n = ()
  | otherwise = error $ funcName ++ ": given invalid " ++ indexName ++ " `" ++ show i ++ "` over " ++ setName ++ " `" ++ show n ++ "`"

-- | \(O(1)\) Asserts \(0 \leq l \leq r \leq n\) for a half-open interval \([l, r)\).
{-# INLINE checkInterval #-}
checkInterval :: (HasCallStack) => String -> Int -> Int -> Int -> ()
checkInterval funcName l r n
  | 0 <= l && l <= r && r <= n = ()
  | otherwise = error $ funcName ++ ": given invalid interval `[" ++ show l ++ ", " ++ show r ++ ")` over length `" ++ show n ++ "`"
