-- | Runtime assertion utility.
module AtCoder.Internal.Assert (runtimeAssert) where

import GHC.Stack (HasCallStack)

-- | Assertion that is not erased at runtime.
runtimeAssert :: (HasCallStack) => Bool -> String -> ()
runtimeAssert p s
  | p = ()
  | otherwise = error s
