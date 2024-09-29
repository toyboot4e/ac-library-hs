-- | Runtime assertion utility.
module AtCoder.Internal.Assert (runtimeAssert) where

-- | Assertion that is not erased at runtime.
runtimeAssert :: Bool -> String -> ()
runtimeAssert p s
  | p = ()
  | otherwise = error s
