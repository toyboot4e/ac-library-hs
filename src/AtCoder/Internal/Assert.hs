-- | Runtime assertion utility.
--
-- ==== __Example__
-- >>> let !_ = runtimeAssert False "errorMessage"
-- *** Exception: errorMessage
-- ...
--
-- >>> let !_ = checkIndex "AtCoder.Internal.Assert.doctest" 0 3
-- >>> let !_ = checkIndex "AtCoder.Internal.Assert.doctest" (-1) 3
-- *** Exception: AtCoder.Internal.Assert.doctest: given invalid index `-1` over length `3`
-- ...
--
-- >>> let !_ = checkIndexBounded "AtCoder.Internal.Assert.doctest" 2 1 3
-- >>> let !_ = checkIndexBounded "AtCoder.Internal.Assert.doctest" (-1) 1 3
-- *** Exception: AtCoder.Internal.Assert.doctest: given invalid index `-1` over bounds `[1, 3)`
-- ...
--
-- >>> let !_ = checkVertex "AtCoder.Internal.Assert.doctest" 0 3
-- >>> let !_ = checkVertex "AtCoder.Internal.Assert.doctest" (-1) 3
-- *** Exception: AtCoder.Internal.Assert.doctest: given invalid vertex `-1` over the number of vertices `3`
-- ...
--
-- >>> let !_ = checkEdge "AtCoder.Internal.Assert.doctest" 0 3
-- >>> let !_ = checkEdge "AtCoder.Internal.Assert.doctest" (-1) 3
-- *** Exception: AtCoder.Internal.Assert.doctest: given invalid edge index `-1` over the number of edges `3`
-- ...
--
-- >>> let !_ = checkCustom "AtCoder.Internal.Assert.doctest" "index" 0 "set" 3
-- >>> let !_ = checkCustom "AtCoder.Internal.Assert.doctest" "index" (-1) "set" 3
-- *** Exception: AtCoder.Internal.Assert.doctest: given invalid index `-1` over set `3`
-- ...
--
-- >>> let !_ = checkInterval "AtCoder.Internal.Assert.doctest" 0 3 3
-- >>> let !_ = checkInterval "AtCoder.Internal.Assert.doctest" 0 4 3
-- *** Exception: AtCoder.Internal.Assert.doctest: given invalid interval `[0, 4)` over length `3`
-- ...
--
-- >>> let !_ = checkIntervalBounded "AtCoder.Internal.Assert.doctest"  2 4 0 5
-- >>> let !_ = checkIntervalBounded "AtCoder.Internal.Assert.doctest" (-1) 0 0 5
-- *** Exception: AtCoder.Internal.Assert.doctest: given invalid interval `[-1, 0)` over bounds `[0, 5)`
-- ...
--
-- @since 1.0.0.0
module AtCoder.Internal.Assert
  ( -- * Runtime assertion
    runtimeAssert,

    -- * Tests
    testIndex,
    testInterval,
    testIntervalBounded,

    -- * Index assertions
    checkIndex,
    errorIndex,
    checkIndexBounded,
    errorIndexBounded,
    checkVertex,
    errorVertex,
    checkEdge,
    errorEdge,
    checkCustom,
    errorCustom,

    -- * Interval assertion
    checkInterval,
    errorInterval,
    checkIntervalBounded,
    errorIntervalBounded,
  )
where

import GHC.Stack (HasCallStack)

-- | \(O(1)\) Assertion that is never erased at compile time.
--
-- @since 1.0.0.0
{-# INLINE runtimeAssert #-}
runtimeAssert :: (HasCallStack) => Bool -> String -> ()
runtimeAssert p s
  | p = ()
  | otherwise = error s

-- | \(O(1)\) Tests \(i \in [0, n)\).
--
-- @since 1.0.0.0
{-# INLINE testIndex #-}
testIndex :: (HasCallStack) => Int -> Int -> Bool
testIndex i n = 0 <= i && i < n

-- | \(O(1)\) Tests whether \([l, r)\) is a valid interval in \([0, n)\).
--
-- @since 1.0.0.0
{-# INLINE testInterval #-}
testInterval :: Int -> Int -> Int -> Bool
testInterval l r n = 0 <= l && l <= r && r <= n

-- | \(O(1)\) Tests whether \([l, r)\) is a valid interval in \([l_0, r_0)\).
--
-- @since 1.2.1.0
{-# INLINE testIntervalBounded #-}
testIntervalBounded :: Int -> Int -> Int -> Int -> Bool
testIntervalBounded l r l0 r0 = l0 <= l && l <= r && r <= r0

-- | \(O(1)\) Asserts \(0 \leq i \lt n\) for an array index \(i\).
--
-- @since 1.0.0.0
{-# INLINE checkIndex #-}
checkIndex :: (HasCallStack) => String -> Int -> Int -> ()
checkIndex funcName i n
  | 0 <= i && i < n = ()
  | otherwise = errorIndex funcName i n

-- | \(O(1)\) Emits index boundary error.
--
-- @since 1.0.0.0
{-# INLINE errorIndex #-}
errorIndex :: (HasCallStack) => String -> Int -> Int -> a
errorIndex funcName i n =
  error $ funcName ++ ": given invalid index `" ++ show i ++ "` over length `" ++ show n ++ "`"

-- | \(O(1)\) Asserts \(l_0 \leq i \lt r_0\) for an array index \(i\).
--
-- @since 1.2.1.0
{-# INLINE checkIndexBounded #-}
checkIndexBounded :: (HasCallStack) => String -> Int -> Int -> Int -> ()
checkIndexBounded funcName i l r
  | l <= i && i < r = ()
  | otherwise = errorIndexBounded funcName i l r

-- | \(O(1)\) Emits index boundary error.
--
-- @since 1.2.1.0
{-# INLINE errorIndexBounded #-}
errorIndexBounded :: (HasCallStack) => String -> Int -> Int -> Int -> a
errorIndexBounded funcName i l r =
  error $ funcName ++ ": given invalid index `" ++ show i ++ "` over bounds `[" ++ show l ++ ", " ++ show r ++ ")`"

-- | \(O(1)\) Asserts \(0 \leq i \lt n\) for a graph vertex \(i\).
--
-- @since 1.0.0.0
{-# INLINE checkVertex #-}
checkVertex :: (HasCallStack) => String -> Int -> Int -> ()
checkVertex funcName i n
  | 0 <= i && i < n = ()
  | otherwise = errorVertex funcName i n

-- | \(O(1)\) Asserts \(0 \leq i \lt n\) for a graph vertex \(i\).
--
-- @since 1.0.0.0
{-# INLINE errorVertex #-}
errorVertex :: (HasCallStack) => String -> Int -> Int -> a
errorVertex funcName i n =
  error $ funcName ++ ": given invalid vertex `" ++ show i ++ "` over the number of vertices `" ++ show n ++ "`"

-- | \(O(1)\) Asserts \(0 \leq i \lt m\) for an edge index \(i\).
--
-- @since 1.0.0.0
{-# INLINE checkEdge #-}
checkEdge :: (HasCallStack) => String -> Int -> Int -> ()
checkEdge funcName i n
  | 0 <= i && i < n = ()
  | otherwise = errorEdge funcName i n

-- | \(O(1)\) Asserts \(0 \leq i \lt m\) for an edge index \(i\).
--
-- @since 1.0.0.0
{-# INLINE errorEdge #-}
errorEdge :: (HasCallStack) => String -> Int -> Int -> a
errorEdge funcName i n =
  error $ funcName ++ ": given invalid edge index `" ++ show i ++ "` over the number of edges `" ++ show n ++ "`"

-- | \(O(1)\) Asserts \(0 \leq i \lt m\) for an edge index \(i\).
--
-- @since 1.0.0.0
{-# INLINE checkCustom #-}
checkCustom :: (HasCallStack) => String -> String -> Int -> String -> Int -> ()
checkCustom funcName indexName i setName n
  | testIndex i n = ()
  | otherwise = errorCustom funcName indexName i setName n

-- | \(O(1)\) Asserts \(0 \leq i \lt m\) for an edge index \(i\).
--
-- @since 1.0.0.0
{-# INLINE errorCustom #-}
errorCustom :: (HasCallStack) => String -> String -> Int -> String -> Int -> a
errorCustom funcName indexName i setName n = error $ funcName ++ ": given invalid " ++ indexName ++ " `" ++ show i ++ "` over " ++ setName ++ " `" ++ show n ++ "`"

-- | \(O(1)\) Asserts \(0 \leq l \leq r \leq n\) for a half-open interval \([l, r)\).
--
-- @since 1.0.0.0
{-# INLINE checkInterval #-}
checkInterval :: (HasCallStack) => String -> Int -> Int -> Int -> ()
checkInterval funcName l r n
  | testInterval l r n = ()
  | otherwise = errorInterval funcName l r n

-- | \(O(1)\) Asserts \(0 \leq l \leq r \leq n\) for a half-open interval \([l, r)\).
--
-- @since 1.0.0.0
{-# INLINE errorInterval #-}
errorInterval :: (HasCallStack) => String -> Int -> Int -> Int -> a
errorInterval funcName l r n = error $ funcName ++ ": given invalid interval `[" ++ show l ++ ", " ++ show r ++ ")` over length `" ++ show n ++ "`"

-- | \(O(1)\) Asserts \(0 \leq l \leq r \leq n\) for a half-open interval \([l, r)\).
--
-- @since 1.2.1.0
{-# INLINE checkIntervalBounded #-}
checkIntervalBounded :: (HasCallStack) => String -> Int -> Int -> Int -> Int -> ()
checkIntervalBounded funcName l r l0 r0
  | testIntervalBounded l r l0 r0 = ()
  | otherwise = errorIntervalBounded funcName l r l0 r0

-- | \(O(1)\) Asserts \(0 \leq l \leq r \leq n\) for a half-open interval \([l, r)\).
--
-- @since 1.2.1.0
{-# INLINE errorIntervalBounded #-}
errorIntervalBounded :: (HasCallStack) => String -> Int -> Int -> Int -> Int -> a
errorIntervalBounded funcName l r l0 r0 = error $ funcName ++ ": given invalid interval `[" ++ show l ++ ", " ++ show r ++ ")` over bounds `[" ++ show l0 ++ ", " ++ show r0 ++ ")`"
