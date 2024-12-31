-- | Minimum parse/print with @bytestring@.
module Util
  ( intP,
    intS1P,
    int2P,
    int3P,
    int4P,
    int,
    ints2,
    ints3,
    ints4,
    ints,
    withLine,
    unlinesBSB,
    unlinesWithBSB,
    unwordsBSB,
    putBSB,
    printBSB,
  )
where

import Control.Monad.Trans.State.Strict (StateT (..), evalStateT)
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Char8 qualified as BS
import Data.Maybe (fromJust, fromMaybe)
import Data.Vector.Generic qualified as VG
import Data.Vector.Unboxed qualified as VU
import GHC.Stack (HasCallStack)
import System.IO (stdout)

-- TODO: use MonadState, add INLINE

-- | Failable parser.
type Parser = StateT BS.ByteString Maybe

-- | Parses an `Int`.
intP :: Parser Int
intP = StateT $ BS.readInt . BS.dropSpace

-- | Parses an `Int` and subtracts @1@.
intS1P :: Parser Int
intS1P = StateT $ ((\(!x, !bs) -> (x - 1, bs)) <$>) . BS.readInt . BS.dropSpace

int2P :: (HasCallStack) => Parser (Int, Int)
int2P = do
  x1 <- intP
  x2 <- intP
  pure (x1, x2)

int3P :: (HasCallStack) => Parser (Int, Int, Int)
int3P = do
  x1 <- intP
  x2 <- intP
  x3 <- intP
  pure (x1, x2, x3)

int4P :: (HasCallStack) => Parser (Int, Int, Int, Int)
int4P = do
  x1 <- intP
  x2 <- intP
  x3 <- intP
  x4 <- intP
  pure (x1, x2, x3, x4)

-- * Line getter

-- | Gets @a@.
int :: (HasCallStack) => IO Int
int = fromMaybe (error "int") . evalStateT intP <$> BS.getLine

-- | Gets @a b@.
ints2 :: (HasCallStack) => IO (Int, Int)
ints2 = fromMaybe (error "ints2") . evalStateT int2P <$> BS.getLine

-- | Gets @a b c@.
ints3 :: (HasCallStack) => IO (Int, Int, Int)
ints3 = fromMaybe (error "ints3") . evalStateT int3P <$> BS.getLine

-- | Gets @a b c d@.
ints4 :: (HasCallStack) => IO (Int, Int, Int, Int)
ints4 = fromMaybe (error "ints4") . evalStateT int4P <$> BS.getLine

-- | Gets @a b c ..@.
ints :: IO (VU.Vector Int)
ints = VU.unfoldr (BS.readInt . BS.dropSpace) <$> BS.getLine

-- | Reads one line from the state and runs a pure parser for it.
withLine :: (HasCallStack) => Parser a -> IO a
withLine f = fromJust . evalStateT f <$> BS.getLine

-- * Handy bytestirng @Builder@

{-# INLINE wsBSB #-}
wsBSB :: BSB.Builder
wsBSB = BSB.char7 ' '

{-# INLINE endlBSB #-}
endlBSB :: BSB.Builder
endlBSB = BSB.char7 '\n'

{-# INLINE intersperseWithBSB #-}
intersperseWithBSB :: (VG.Vector v a) => (a -> BSB.Builder) -> BSB.Builder -> v a -> BSB.Builder
intersperseWithBSB showF del vec
  | VG.null vec = mempty
  | otherwise = showF (VG.head vec) <> VG.foldMap ((del <>) . showF) (VG.tail vec)

{-# INLINE unwordsBSB #-}
unwordsBSB :: VU.Vector Int -> BSB.Builder
unwordsBSB = intersperseWithBSB BSB.intDec wsBSB

{-# INLINE unlinesBSB #-}
unlinesBSB :: VU.Vector Int -> BSB.Builder
unlinesBSB = intersperseWithBSB BSB.intDec endlBSB

{-# INLINE unlinesWithBSB #-}
unlinesWithBSB :: (VG.Vector v a) => (a -> BSB.Builder) -> v a -> BSB.Builder
unlinesWithBSB showF = intersperseWithBSB showF endlBSB

{-# INLINE putBSB #-}
putBSB :: BSB.Builder -> IO ()
putBSB = BSB.hPutBuilder stdout

{-# INLINE printBSB #-}
printBSB :: BSB.Builder -> IO ()
printBSB = putBSB . (<> endlBSB)
