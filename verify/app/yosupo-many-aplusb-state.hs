import Control.Monad.State.Class
import Control.Monad.Trans.State.Strict (State (..), StateT (..), evalState, evalStateT)
import Data.ByteString.Char8 qualified as BS
import Data.Maybe (fromJust)
import Data.Vector.Unboxed qualified as VU
import GHC.Stack (HasCallStack)
import Util

{-# INLINE int' #-}
int' :: (HasCallStack) => State BS.ByteString Int
int' = state $ fromJust . BS.readInt . BS.dropSpace

-- somehow bit evalState is slow

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/many_aplusb
main :: IO ()
main = do
  bs <- BS.getContents
  let !res = (`evalState` bs) $ do
        !t <- int'
        VU.replicateM t $ do
          !x <- (+) <$> int' <*> int'
          pure x
  printBSB $ unwordsBSB res
