-- | Minimum parse/print with @bytestring@.
module Util (ints2, ints3, unlinesBSB, unwordsBSB, putBSB, printBSB) where

import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BS
import Control.Monad.Trans.State.Strict (evalStateT, StateT(..), get)
import Data.Maybe (fromMaybe)
import Data.Vector.Generic qualified as VG
import Data.Vector.Unboxed qualified as VU
import System.IO (stdout)
import Debug.Trace

-- type Parser = StateT BS.ByteString Maybe

-- | Parses @a b@.
ints2 :: IO (Int, Int)
ints2 = do
  line <- BS.getLine
  return . fromMaybe (error "ints2") . (`evalStateT` line) $ do
    x1 <- StateT $ BS.readInt . BS.dropSpace
    x2 <- StateT $ BS.readInt . BS.dropSpace
    return (x1, x2)

-- | Parses @a b c@.
ints3 :: IO (Int, Int, Int)
ints3 = do
  line <- BS.getLine
  return . fromMaybe (error "ints3") . (`evalStateT` line) $ do
    x1 <- StateT $ BS.readInt . BS.dropSpace
    x2 <- StateT $ BS.readInt . BS.dropSpace
    x3 <- StateT $ BS.readInt . BS.dropSpace
    return (x1, x2, x3)

{-# INLINE wsBSB #-}
wsBSB :: BSB.Builder
wsBSB = BSB.char7 ' '

{-# INLINE endlBSB #-}
endlBSB :: BSB.Builder
endlBSB = BSB.char7 '\n'

{-# INLINE intersperseBSB #-}
intersperseBSB :: (VG.Vector v Int) => BSB.Builder -> v Int -> BSB.Builder
intersperseBSB del vec
  | VG.null vec = mempty
  | otherwise = BSB.intDec (VG.head vec) <> VG.foldMap ((del <>) . BSB.intDec) (VG.tail vec)

{-# INLINE unwordsBSB #-}
unwordsBSB :: VU.Vector Int -> BSB.Builder
unwordsBSB = intersperseBSB wsBSB

{-# INLINE unlinesBSB #-}
unlinesBSB :: VU.Vector Int -> BSB.Builder
unlinesBSB = intersperseBSB endlBSB

{-# INLINE putBSB #-}
putBSB :: BSB.Builder -> IO ()
putBSB = BSB.hPutBuilder stdout

{-# INLINE printBSB #-}
printBSB :: BSB.Builder -> IO ()
printBSB = putBSB . (<> endlBSB)

