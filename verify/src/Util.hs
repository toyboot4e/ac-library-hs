-- | Minimum parse/print with @bytestring@.
module Util (int, ints2, ints3, ints4, ints, unlinesBSB, unwordsBSB, putBSB, printBSB) where

import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BS
import Control.Monad.Trans.State.Strict (evalStateT, StateT(..))
import Data.Maybe (fromMaybe)
import Data.Vector.Generic qualified as VG
import Data.Vector.Unboxed qualified as VU
import System.IO (stdout)

-- | Failable parser.
type Parser = StateT BS.ByteString Maybe

-- | Parses an `Int`.
intP :: Parser Int
intP = StateT $ BS.readInt . BS.dropSpace

-- | Parses @a@.
int :: IO Int
int = do
  line <- BS.getLine
  return . fromMaybe (error "int") . (`evalStateT` line) $ do
    intP

-- | Parses @a b@.
ints2 :: IO (Int, Int)
ints2 = do
  line <- BS.getLine
  return . fromMaybe (error "ints2") . (`evalStateT` line) $ do
    x1 <- intP
    x2 <- intP
    return (x1, x2)

-- | Parses @a b c@.
ints3 :: IO (Int, Int, Int)
ints3 = do
  line <- BS.getLine
  return . fromMaybe (error "ints3") . (`evalStateT` line) $ do
    x1 <- intP
    x2 <- intP
    x3 <- intP
    return (x1, x2, x3)

-- | Parses @a b c d@.
ints4 :: IO (Int, Int, Int, Int)
ints4 = do
  line <- BS.getLine
  return . fromMaybe (error "ints4") . (`evalStateT` line) $ do
    x1 <- intP
    x2 <- intP
    x3 <- intP
    x4 <- intP
    return (x1, x2, x3, x4)

-- | Parses @a b c ..@.
ints :: IO (VU.Vector Int)
ints = VU.unfoldr (BS.readInt . BS.dropSpace) <$> BS.getLine

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

