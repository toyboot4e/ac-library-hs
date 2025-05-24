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
-- >>> let !_ = checkPoint2d "AtCoder.Internal.Assert.doctest"  1 1 2 2
-- >>> let !_ = checkPoint2d "AtCoder.Internal.Assert.doctest" 4 4 2 2
-- *** Exception: AtCoder.Internal.Assert.doctest: given invalid point `(4, 4)` for rectangle `[0, 2) x [0, 2)`
-- ...
--
-- >>> let !_ = checkRect "AtCoder.Internal.Assert.doctest"  1 2 1 2 3 3
-- >>> let !_ = checkRect "AtCoder.Internal.Assert.doctest" 1 2 1 2 1 1
-- *** Exception: AtCoder.Internal.Assert.doctest: given invalid rectangle `[1, 2) x [1, 2)` for rectangle `[0, 1) x [0, 1)`
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
    testPoint2d,
    testRect,
    testRectShape,

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

    -- * Interval assertions
    checkInterval,
    errorInterval,
    checkIntervalBounded,
    errorIntervalBounded,

    -- * Two-dimensional index assertions
    checkPoint2d,
    errorPoint2d,
    checkRect,
    errorRect,
    checkRectShape,
    errorRectShape,
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

-- | \(O(1)\) Tests \(0 \le i \lt n\).
--
-- @since 1.0.0.0
{-# INLINE testIndex #-}
testIndex ::
  (HasCallStack) =>
  -- | \(i\)
  Int ->
  -- | \(n\)
  Int ->
  -- | \(0 \le i \lt n\)
  Bool
testIndex i n = 0 <= i && i < n

-- | \(O(1)\) Tests \(0 \le l \le r \le n\).
--
-- @since 1.0.0.0
{-# INLINE testInterval #-}
testInterval ::
  -- | \(l\)
  Int ->
  -- | \(r\)
  Int ->
  -- | \(n\)
  Int ->
  -- | \(0 \le l \le r \le n\).
  Bool
testInterval l r n = 0 <= l && l <= r && r <= n

-- | \(O(1)\) Tests \(l_0 \le l \le r \le r_0\).
--
-- @since 1.2.1.0
{-# INLINE testIntervalBounded #-}
testIntervalBounded ::
  -- | \(l\)
  Int ->
  -- | \(r\)
  Int ->
  -- | \(l_0\)
  Int ->
  -- | \(r_0\)
  Int ->
  -- | \(l_0 \le l \le r \le r_0\)
  Bool
testIntervalBounded l r l0 r0 = l0 <= l && l <= r && r <= r0

-- | \(O(1)\) Tests \((x, y) \in [0, w) \times [0, h)\).
--
-- @since 1.2.3.0
{-# INLINE testPoint2d #-}
testPoint2d ::
  (HasCallStack) =>
  -- | \(x\)
  Int ->
  -- | \(y\)
  Int ->
  -- | \(w\)
  Int ->
  -- | \(h\)
  Int ->
  -- | \((x, y) \in [0, w) \times [0, h)\)
  Bool
testPoint2d x y w h = 0 <= x && x < w && 0 <= y && y < h

-- | \(O(1)\) Tests \([x_1, x_2) \times [y_1 y_2) \in [0, w) \times [0, h)\).
--
-- @since 1.2.3.0
{-# INLINE testRect #-}
testRect ::
  (HasCallStack) =>
  -- | \(x_1\)
  Int ->
  -- | \(x_2\)
  Int ->
  -- | \(y_1\)
  Int ->
  -- | \(y_2\)
  Int ->
  -- | \(w\)
  Int ->
  -- | \(h\)
  Int ->
  -- | \([x_1, x_2) \times [y_1 y_2) \in [0, w) \times [0, h)\).
  Bool
testRect x1 x2 y1 y2 w h = 0 <= x1 && x1 <= x2 && x2 <= w && 0 <= y1 && y1 <= y2 && y2 <= h

-- | \(O(1)\) Tests \(x_1 \le x_2 \land y_1 \le \y_2\).
--
-- @since 1.2.3.0
{-# INLINE testRectShape #-}
testRectShape ::
  (HasCallStack) =>
  -- | \(x_1\)
  Int ->
  -- | \(x_2\)
  Int ->
  -- | \(y_1\)
  Int ->
  -- | \(y_2\)
  Int ->
  -- | \(x_1 \le x_2 \land y_1 \le \y_2\).
  Bool
testRectShape x1 x2 y1 y2 = x1 <= x2 && y1 <= y2

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

-- | \(O(1)\) Emits vertex boundary error.
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

-- | \(O(1)\) Emits edge index boundary error.
--
-- @since 1.0.0.0
{-# INLINE errorEdge #-}
errorEdge :: (HasCallStack) => String -> Int -> Int -> a
errorEdge funcName i n =
  error $ funcName ++ ": given invalid edge index `" ++ show i ++ "` over the number of edges `" ++ show n ++ "`"

-- | \(O(1)\) Asserts index boundary with custom message.
--
-- @since 1.0.0.0
{-# INLINE checkCustom #-}
checkCustom :: (HasCallStack) => String -> String -> Int -> String -> Int -> ()
checkCustom funcName indexName i setName n
  | testIndex i n = ()
  | otherwise = errorCustom funcName indexName i setName n

-- | \(O(1)\) Emis custom index error.
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

-- | \(O(1)\) Emits interval boundary error.
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

-- | \(O(1)\) Emits interval boundary error.
--
-- @since 1.2.1.0
{-# INLINE errorIntervalBounded #-}
errorIntervalBounded :: (HasCallStack) => String -> Int -> Int -> Int -> Int -> a
errorIntervalBounded funcName l r l0 r0 = error $ funcName ++ ": given invalid interval `[" ++ show l ++ ", " ++ show r ++ ")` over bounds `[" ++ show l0 ++ ", " ++ show r0 ++ ")`"

-- | \(O(1)\) Asserts \(0 \leq i \lt n\) for a graph vertex \(i\).
--
-- @since 1.2.3.0
{-# INLINE checkPoint2d #-}
checkPoint2d :: (HasCallStack) => String -> Int -> Int -> Int -> Int -> ()
checkPoint2d funcName x y w h
  | testPoint2d x y w h = ()
  | otherwise = errorPoint2d funcName x y w h

-- | \(O(1)\) Emits point boundary error.
--
-- @since 1.2.3.0
{-# INLINE errorPoint2d #-}
errorPoint2d :: (HasCallStack) => String -> Int -> Int -> Int -> Int -> a
errorPoint2d funcName x y w h =
  error $ funcName ++ ": given invalid point `(" ++ show x ++ ", " ++ show y ++ ")` for rectangle `[0, " ++ show w ++ ") x [0, " ++ show h ++ ")`"

-- | \(O(1)\) Asserts \([x_1, x_2) \times [y_1 y_2) \in [0, w) \times [0, h)\).
--
-- @since 1.2.3.0
{-# INLINE checkRect #-}
checkRect :: (HasCallStack) => String -> Int -> Int -> Int -> Int -> Int -> Int -> ()
checkRect funcName x1 x2 y1 y2 w h
  | testRect x1 x2 y1 y2 w h = ()
  | otherwise = errorRect funcName x1 x2 y1 y2 w h

-- | \(O(1)\) Asserts rectangle boundary error.
--
-- @since 1.2.3.0
{-# INLINE errorRect #-}
errorRect :: (HasCallStack) => String -> Int -> Int -> Int -> Int -> Int -> Int -> a
errorRect funcName x1 x2 y1 y2 w h =
  error $ funcName ++ ": given invalid rectangle `[" ++ show x1 ++ ", " ++ show x2 ++ ") x [" ++ show y1 ++ ", " ++ show y2 ++ ")` for rectangle `[0, " ++ show w ++ ") x [0, " ++ show h ++ ")`"

-- | \(O(1)\) Asserts \(x_1 \le x_2\) and \(y_1 \le y_2\).
--
-- @since 1.2.3.0
{-# INLINE checkRectShape #-}
checkRectShape :: (HasCallStack) => String -> Int -> Int -> Int -> Int -> ()
checkRectShape funcName x1 x2 y1 y2
  | testRectShape x1 x2 y1 y2 = ()
  | otherwise = errorRectShape funcName x1 x2 y1 y2

-- | \(O(1)\) Asserts rectangle boundary error.
--
-- @since 1.2.3.0
{-# INLINE errorRectShape #-}
errorRectShape :: (HasCallStack) => String -> Int -> Int -> Int -> Int -> a
errorRectShape funcName x1 x2 y1 y2 =
  error $ funcName ++ ": given invalid rectangle `[" ++ show x1 ++ ", " ++ show x2 ++ ") x [" ++ show y1 ++ ", " ++ show y2 ++ ")`"
