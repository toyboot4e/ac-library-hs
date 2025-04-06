import Control.Monad.Trans.State.Strict (StateT (..), evalStateT)
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Char8 qualified as BS
import Data.Maybe (fromMaybe)
import Data.Vector.Generic qualified as VG
import Data.Vector.Unboxed qualified as VU
import GHC.Stack (HasCallStack)
import System.IO (stdout)
import Util

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/many_aplusb
main :: IO ()
main = do
  t <- int
  res <- VU.replicateM t $ do
    (!a, !b) <- ints2
    pure $! a + b
  printBSB $ unlinesBSB res
