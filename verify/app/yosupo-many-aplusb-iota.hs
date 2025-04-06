import Control.Applicative (Applicative (..))
import Control.Monad
import Control.Monad.State.Strict
import Data.ByteString qualified as B
import Data.ByteString.Builder qualified as B
import Data.ByteString.Internal qualified as B
import Data.Function
import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable qualified as GM
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM
import Foreign
import GHC.Exts
import GHC.Word
import Iota.Prelude
import Iota.PrimParser
import System.IO

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/many_aplusb
main :: IO ()
main = runSolver (putBuilder . unlinesB B.intDec) $ do
  t <- int
  uvectorN t $ do
    a <- int
    b <- int
    pure $! a + b
