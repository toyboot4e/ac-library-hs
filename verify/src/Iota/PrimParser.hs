{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

-- | From `https://github.com/cojna/iota` (CC0 license)
module Iota.PrimParser where

import Control.Applicative (Applicative (..))
import Control.Monad
import Control.Monad.ST
import Data.ByteString qualified as B
import Data.ByteString.Internal qualified as B
import Data.ByteString.Short qualified as B (ShortByteString)
import Data.ByteString.Short qualified as B.Short
import Data.Function
import Data.Primitive
import Foreign
import GHC.Exts
import GHC.Word
import System.IO
import Prelude hiding (Applicative (..))

newtype PrimParser a = PrimParser
  { runPrimParser# ::
      Addr# ->
      Addr# ->
      (# Addr#, a #)
  }

instance Functor PrimParser where
  fmap f mx = PrimParser $ \e p -> case runPrimParser# mx e p of
    (# p', x #) -> (# p', f x #)
  {-# INLINE fmap #-}

instance Applicative PrimParser where
  pure x = PrimParser $ \_ p -> (# p, x #)
  {-# INLINE pure #-}
  mf <*> mx = PrimParser $ \e p -> case runPrimParser# mf e p of
    (# p', f #) -> case runPrimParser# mx e p' of
      (# p'', x #) -> (# p'', f x #)
  {-# INLINE (<*>) #-}
  liftA2 f mx my = PrimParser $ \e p -> case runPrimParser# mx e p of
    (# p', x #) -> case runPrimParser# my e p' of
      (# p'', y #) -> (# p'', f x y #)
  {-# INLINE liftA2 #-}

instance Monad PrimParser where
  mx >>= f = PrimParser $ \e p -> case runPrimParser# mx e p of
    (# p', x #) -> runPrimParser# (f x) e p'
  {-# INLINE (>>=) #-}

runPrimParser :: PrimParser a -> Ptr Word8 -> Ptr Word8 -> (a, Ptr Word8)
runPrimParser f (Ptr e#) (Ptr p#) = case runPrimParser# f e# p# of
  (# p', x #) -> (x, Ptr p')
{-# INLINE runPrimParser #-}

withByteString :: B.ByteString -> (a -> IO r) -> PrimParser a -> IO r
withByteString bs k f = case B.toForeignPtr bs of
  (fp, o, I# len#) -> do
    withForeignPtr (plusForeignPtr fp o) $ \(Ptr p#) -> do
      case runPrimParser# f (plusAddr# p# len#) p# of
        (# _, x #) -> k x

unsafeWithByteString :: B.ByteString -> PrimParser a -> a
unsafeWithByteString bs f =
  B.accursedUnutterablePerformIO $
    withByteString bs return f

withInputHandle :: Handle -> (a -> IO r) -> PrimParser a -> IO r
withInputHandle h k f = do
  bs <- B.hGetContents h
  withByteString bs k f

withInputHandleUpToN :: Handle -> Int -> (a -> IO r) -> PrimParser a -> IO r
withInputHandleUpToN h bufSize k f = do
  buf <- mallocForeignPtrBytes bufSize
  withForeignPtr buf $ \ibuf@(Ptr ibuf#) -> do
    isize@(I# isize#) <- hGetBufNonBlocking h ibuf bufSize
    pokeElemOff @Word8 (plusPtr ibuf isize) 0 10 -- sentinel ('\n')
    case runPrimParser# f (plusAddr# ibuf# (isize# +# 1#)) ibuf# of
      (# _, x #) -> k x

viewPrimParser :: PrimParser (Ptr Word8, Ptr Word8)
viewPrimParser = PrimParser $ \e p ->
  (# p, (Ptr e, Ptr p) #)

viewPrimParserAsByteString :: PrimParser B.ByteString
viewPrimParserAsByteString = PrimParser $ \e p ->
  let n = I# (minusAddr# e p)
      bs = B.unsafeCreate n $ \dst ->
        copyBytes dst (Ptr p) n
   in (# p, bs #)

wordP# :: Addr# -> Word# -> (# Addr#, Word# #)
wordP# p# acc# = case word8ToWord# (indexWord8OffAddr# p# 0#) of
  c
    | isTrue# (geWord# c 0x30##) ->
        wordP#
          (plusAddr# p# 1#)
          (plusWord# (and# c 0x0f##) (timesWord# 10## acc#))
    | otherwise -> (# p#, acc# #)

intP :: PrimParser Int
intP = PrimParser $ \_ p ->
  case word8ToWord# (indexWord8OffAddr# p 0#) of
    0x2d## -> case wordP# (plusAddr# p 1#) 0## of
      (# p', w #) -> (# p', I# (negateInt# (word2Int# w)) #)
    c -> case wordP# (plusAddr# p 1#) (and# c 0xf##) of
      (# p', w #) -> (# p', I# (word2Int# w) #)
{-# INLINE intP #-}

wordP :: PrimParser Word
wordP = PrimParser $ \_ p ->
  case wordP# p 0## of
    (# p', w #) -> (# p', W# w #)
{-# INLINE wordP #-}

doubleP :: PrimParser Double
doubleP = PrimParser $ \_ p ->
  case word8ToWord# (indexWord8OffAddr# p 0#) of
    0x2d## -> case unsignedDoubleP# (plusAddr# p 1#) 0## of
      (# p', d #) -> (# p', D# (negateDouble# d) #)
    c -> case unsignedDoubleP# (plusAddr# p 1#) (and# c 0xf##) of
      (# p', d #) -> (# p', D# d #)

unsignedDoubleP# :: Addr# -> Word# -> (# Addr#, Double# #)
unsignedDoubleP# p w = case wordP# p w of
  (# p', iw #) -> case word8ToWord# (indexWord8OffAddr# p' 0#) of
    0x2e## -> case wordP# (plusAddr# p' 1#) 0## of
      (# p'', fw #) ->
        let ipart = word2Double# iw
            fpart = word2Double# fw /## indexDoubleOffAddr# pow10# (minusAddr# p'' p' -# 1#)
         in (# p'', ipart +## fpart #)
    _ -> (# p', word2Double# iw #)
    where
      pow10# = "\x00\x00\x00\x00\x00\x00\xf0\x3f\x00\x00\x00\x00\x00\x00\x24\x40\x00\x00\x00\x00\x00\x00\x59\x40\x00\x00\x00\x00\x00\x40\x8f\x40\x00\x00\x00\x00\x00\x88\xc3\x40\x00\x00\x00\x00\x00\x6a\xf8\x40\x00\x00\x00\x00\x80\x84\x2e\x41\x00\x00\x00\x00\xd0\x12\x63\x41\x00\x00\x00\x00\x84\xd7\x97\x41\x00\x00\x00\x00\x65\xcd\xcd\x41\x00\x00\x00\x20\x5f\xa0\x02\x42\x00\x00\x00\xe8\x76\x48\x37\x42\x00\x00\x00\xa2\x94\x1a\x6d\x42\x00\x00\x40\xe5\x9c\x30\xa2\x42\x00\x00\x90\x1e\xc4\xbc\xd6\x42\x00\x00\x34\x26\xf5\x6b\x0c\x43\x00\x80\xe0\x37\x79\xc3\x41\x43"#
{-# INLINE unsignedDoubleP# #-}

int :: PrimParser Int
int = PrimParser $ \e p ->
  case runPrimParser# intP e p of
    (# p', x #) -> (# plusAddr# p' 1#, x #)
{-# INLINE int #-}

uint :: PrimParser Int
uint = PrimParser $ \_ p ->
  case wordP# p 0## of
    (# p', x #) -> (# plusAddr# p' 1#, I# (word2Int# x) #)
{-# INLINE uint #-}

uint1 :: PrimParser Int
uint1 = PrimParser $ \_ p ->
  case wordP# p 0## of
    (# p', x #) -> (# plusAddr# p' 1#, I# (word2Int# x -# 1#) #)
{-# INLINE uint1 #-}

double :: PrimParser Double
double = PrimParser $ \e p ->
  case runPrimParser# doubleP e p of
    (# p', x #) -> (# plusAddr# p' 1#, x #)
{-# INLINE double #-}

byte :: PrimParser Word8
byte = PrimParser $ \_ p ->
  case indexWord8OffAddr# p 0# of
    w8 -> (# plusAddr# p 1#, W8# w8 #)
{-# INLINE byte #-}

char :: PrimParser Char
char = PrimParser $ \_ p ->
  case indexWord8OffAddr# p 0# of
    w8 -> (# plusAddr# p 1#, B.w2c (W8# w8) #)
{-# INLINE char #-}

charSp :: PrimParser Char
charSp = PrimParser $ \_ p ->
  case indexWord8OffAddr# p 0# of
    w8 -> (# plusAddr# p 2#, B.w2c (W8# w8) #)
{-# INLINE charSp #-}

charLn :: PrimParser Char
charLn = PrimParser $ \_ p ->
  case indexWord8OffAddr# p 0# of
    w8 -> (# plusAddr# p 2#, B.w2c (W8# w8) #)
{-# INLINE charLn #-}

digitC :: PrimParser Int
digitC = PrimParser $ \_ p ->
  case indexWord8OffAddr# p 0# of
    w -> (# plusAddr# p 1#, I# (word2Int# (word8ToWord# w) -# 0x30#) #)
{-# INLINE digitC #-}

lowerC :: PrimParser Int
lowerC = PrimParser $ \_ p ->
  case indexWord8OffAddr# p 0# of
    w -> (# plusAddr# p 1#, I# (word2Int# (word8ToWord# w) -# 0x61#) #)
{-# INLINE lowerC #-}

upperC :: PrimParser Int
upperC = PrimParser $ \_ p ->
  case indexWord8OffAddr# p 0# of
    w -> (# plusAddr# p 1#, I# (word2Int# (word8ToWord# w) -# 0x41#) #)
{-# INLINE upperC #-}

memchrP# :: Addr# -> Addr# -> Word8 -> Addr#
memchrP# e p w8 =
  let !(Ptr pos) =
        B.accursedUnutterablePerformIO $
          B.memchr (Ptr p) w8 (fromIntegral (I# (minusAddr# e p)))
   in pos

memchrNthP# :: Int# -> Addr# -> Addr# -> Word8 -> Addr#
memchrNthP# n e ptr w8 = go n ptr
  where
    go i p
      | isTrue# (i ># 1#) = go (i -# 1#) (plusAddr# (memchrP# e p w8) 1#)
      | otherwise = memchrP# e p w8

byteStringTo :: Word8 -> PrimParser B.ByteString
byteStringTo w = PrimParser $ \e p ->
  let !end = memchrP# e p w
      len = I# (minusAddr# end p)
      bs = B.unsafeCreate len $ \dst ->
        copyBytes dst (Ptr p) len
   in (# plusAddr# end 1#, bs #)
{-# INLINE byteStringTo #-}

byteStringLn :: PrimParser B.ByteString
byteStringLn = byteStringTo 0xa
{-# INLINE byteStringLn #-}

byteStringSp :: PrimParser B.ByteString
byteStringSp = byteStringTo 0x20
{-# INLINE byteStringSp #-}

byteStringN :: Int -> PrimParser B.ByteString
byteStringN n@(I# n#) = PrimParser $ \_ p ->
  let bs = B.unsafeCreate n $ \dst ->
        copyBytes dst (Ptr p) n
   in (# plusAddr# p n#, bs #)
{-# INLINE byteStringN #-}

byteStringHW :: Int -> Int -> PrimParser B.ByteString
byteStringHW h@(I# h#) w@(I# w#) = PrimParser $ \_ p ->
  let bs = B.unsafeCreate (h * w) $ \dst0 ->
        fix
          ( \loop !dst !src !i -> when (i < h) $ do
              copyBytes dst src w
              loop (plusPtr dst w) (plusPtr src (w + 1)) (i + 1)
          )
          dst0
          (Ptr p)
          0
   in (# plusAddr# p (h# *# (w# +# 1#)), bs #)
{-# INLINE byteStringHW #-}

byteArrayTo :: Word8 -> PrimParser ByteArray
byteArrayTo w = PrimParser $ \e p ->
  let !end = memchrP# e p w
      n = I# (minusAddr# end p)
      ba = runST $ do
        buf <- newByteArray n
        copyPtrToMutableByteArray @_ @Word8 buf 0 (Ptr p) n
        freezeByteArray buf 0 n
   in (# plusAddr# end 1#, ba #)
{-# INLINE byteArrayTo #-}

byteArrayLn :: PrimParser ByteArray
byteArrayLn = byteArrayTo 0xa
{-# INLINE byteArrayLn #-}

byteArraySp :: PrimParser ByteArray
byteArraySp = byteArrayTo 0x20
{-# INLINE byteArraySp #-}

byteArrayN :: Int -> PrimParser ByteArray
byteArrayN n@(I# n#) = PrimParser $ \_ p ->
  let !ba = runST $ do
        buf <- newByteArray n
        copyPtrToMutableByteArray @_ @Word8 buf 0 (Ptr p) n
        freezeByteArray buf 0 n
   in (# plusAddr# p n#, ba #)
{-# INLINE byteArrayN #-}

byteArrayHW :: Int -> Int -> PrimParser ByteArray
byteArrayHW h@(I# h#) w@(I# w#) = PrimParser $ \_ p ->
  let !ba = runST $ do
        buf <- newByteArray (h * w)
        fix
          ( \loop !src !i -> when (i < h) $ do
              copyPtrToMutableByteArray @_ @Word8 buf (i * w) src w
              loop (plusPtr src (w + 1)) (i + 1)
          )
          (Ptr p)
          0
        freezeByteArray buf 0 (h * w)
   in (# plusAddr# p (h# *# (w# +# 1#)), ba #)
{-# INLINE byteArrayHW #-}

shortByteStringTo :: Word8 -> PrimParser B.ShortByteString
shortByteStringTo w = PrimParser $ \e p ->
  let !end = memchrP# e p w
      n = I# (minusAddr# end p)
      !(ByteArray ba#) = runST $ do
        buf <- newByteArray n
        copyPtrToMutableByteArray @_ @Word8 buf 0 (Ptr p) n
        freezeByteArray buf 0 n
   in (# plusAddr# end 1#, B.Short.SBS ba# #)
{-# INLINE shortByteStringTo #-}

shortByteStringLn :: PrimParser B.ShortByteString
shortByteStringLn = shortByteStringTo 0xa
{-# INLINE shortByteStringLn #-}

shortByteStringSp :: PrimParser B.ShortByteString
shortByteStringSp = shortByteStringTo 0x20
{-# INLINE shortByteStringSp #-}

line :: PrimParser a -> PrimParser a
line f = PrimParser $ \e p ->
  case plusAddr# (memchrP# e p 0xa) 1# of
    pos -> case runPrimParser# f pos p of
      (# _, x #) -> (# pos, x #)
{-# INLINE line #-}

linesN :: Int -> PrimParser a -> PrimParser a
linesN (I# n#) f = PrimParser $ \e p ->
  if isTrue# (n# ># 0#)
    then case plusAddr# (memchrNthP# n# e p 0xa) 1# of
      pos -> case runPrimParser# f pos p of
        (# _, x #) -> (# pos, x #)
    else case runPrimParser# f p p of
      (# _, x #) -> (# p, x #)
{-# INLINE linesN #-}
