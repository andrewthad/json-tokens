{-# language BangPatterns #-}
{-# language BinaryLiterals #-}
{-# language DeriveFunctor #-}
{-# language DerivingStrategies #-}
{-# language DuplicateRecordFields #-}
{-# language LambdaCase #-}
{-# language MagicHash #-}
{-# language PolyKinds #-}
{-# language RankNTypes #-}
{-# language StandaloneDeriving #-}
{-# language TypeApplications #-}
{-# language UnboxedTuples #-}

module Data.Json.Tokenize
  ( Token(..)
  , JsonTokenizeException(..)
  , decode
  ) where

import Control.Monad.ST (ST,runST)
import Data.Bits ((.&.),(.|.),unsafeShiftR)
import Data.Builder.ST (Builder)
import Data.Bytes.Parser (Parser)
import Data.Bytes.Types (Bytes(..))
import Data.Char (ord)
import Data.Int (Int64)
import Data.Primitive (MutableByteArray,ByteArray)
import Data.Primitive (SmallArray)
import Data.Text.Short (ShortText)
import Data.Word (Word8,Word16,Word32)
import GHC.Int (Int64(I64#))
import GHC.Exts (Char#,Int#,Int(I#),Char(C#))
import GHC.Exts (word2Int#,chr#,gtWord#,ltWord#)
import GHC.Word (Word16(W16#),Word8(W8#))
import Data.Number.Scientific (Scientific)

import qualified Data.ByteString.Short.Internal as BSS
import qualified Data.Bytes.Parser as P
import qualified Data.Bytes.Parser.Utf8 as Utf8
import qualified Data.Bytes.Parser.Latin as Latin
import qualified Data.Bytes.Parser.Unsafe as Unsafe
import qualified Data.Builder.ST as B
import qualified Data.Chunks as C
import qualified Data.Primitive as PM
import qualified Data.Text.Short.Unsafe as TS
import qualified Data.Number.Scientific as SCI

data Token
  = LeftBrace
  | RightBrace
  | LeftBracket
  | RightBracket
  | Comma
  | Colon
  | BooleanTrue
  | BooleanFalse
  | Null
  | String {-# UNPACK #-} !ShortText
  | Number {-# UNPACK #-} !Scientific
  deriving stock (Eq,Show)

data JsonTokenizeException
  = NonNumericCharacter
  | NonTrueCharacter
  | NonFalseCharacter
  | NonNullCharacter
  | NonLeadingCharacter
  | BadEscapeSequence
  | JsonTokenizeException
  deriving stock (Eq,Show)

data SmallNumber = SmallNumber
  { coefficient :: {-# UNPACK #-} !Int64
  , exponent :: {-# UNPACK #-} !Int64
  }
  deriving stock (Eq,Show)

data LargeNumber = LargeNumber
  { coefficient :: !Integer
  , exponent :: !Integer
  }
  deriving stock (Eq,Show)

isSpace :: Word8 -> Bool
isSpace w =
     w == c2w ' '
  || w == c2w '\t'
  || w == c2w '\r'
  || w == c2w '\n'

decode :: Bytes -> Either JsonTokenizeException (SmallArray Token)
decode !bs = runST $ do
  !b <- B.new
  P.parseBytesST (P.skipWhile isSpace *> manyTokens b) bs >>= \case
    P.Failure err -> pure (Left err)
    P.Success cs _ -> pure (Right cs)

manyTokens ::
     Builder s Token
  -> Parser JsonTokenizeException s (SmallArray Token)
manyTokens !b0 = do
  t <- oneToken
  !b1 <- P.effect (B.push t b0)
  P.skipWhile isSpace
  done <- P.isEndOfInput
  if done
    then P.effect $ do
      cs <- B.freeze b1
      pure $! C.concat cs
    else manyTokens b1

oneToken :: Parser JsonTokenizeException s Token
oneToken = Latin.any JsonTokenizeException >>= \case
  '{' -> pure LeftBrace
  '}' -> pure RightBrace
  '[' -> pure LeftBracket
  ']' -> pure RightBracket
  ',' -> pure Comma
  ':' -> pure Colon
  't' -> do
    Latin.char3 NonTrueCharacter 'r' 'u' 'e'
    pure BooleanTrue
  'f' -> do
    Latin.char4 NonFalseCharacter 'a' 'l' 's' 'e'
    pure BooleanFalse
  'n' -> do
    Latin.char3 NonNullCharacter 'u' 'l' 'l'
    pure Null
  '"' -> do
    start <- Unsafe.cursor
    string start
  '-' -> fmap Number (SCI.parserNegatedUtf8Bytes NonLeadingCharacter)
  c | c >= '0' && c <= '9' -> do
        Unsafe.unconsume 1
        fmap Number (SCI.parserUnsignedUtf8Bytes NonLeadingCharacter)
  _ -> P.fail NonLeadingCharacter

copyAndEscape :: Int -> Parser JsonTokenizeException s Token
copyAndEscape !maxLen = do
  !dst <- P.effect (PM.newByteArray maxLen)
  let go !ix = Utf8.any# JsonTokenizeException `P.bindFromCharToLifted` \c -> case c of
        '\\'# -> Latin.any JsonTokenizeException >>= \case
          '"' -> do
            P.effect (PM.writeByteArray dst ix (c2w '"'))
            go (ix + 1)
          '\\' -> do
            P.effect (PM.writeByteArray dst ix (c2w '\\'))
            go (ix + 1)
          't' -> do
            P.effect (PM.writeByteArray dst ix (c2w '\t'))
            go (ix + 1)
          'n' -> do
            P.effect (PM.writeByteArray dst ix (c2w '\n'))
            go (ix + 1)
          'r' -> do
            P.effect (PM.writeByteArray dst ix (c2w '\r'))
            go (ix + 1)
          '/' -> do
            P.effect (PM.writeByteArray dst ix (c2w '/'))
            go (ix + 1)
          'b' -> do
            P.effect (PM.writeByteArray dst ix (c2w '\b'))
            go (ix + 1)
          'f' -> do
            P.effect (PM.writeByteArray dst ix (c2w '\f'))
            go (ix + 1)
          'u' -> do
            w <- Latin.hexWord16 JsonTokenizeException
            if w >= 0xD800 && w < 0xDFFF
              then go =<< P.effect (encodeUtf8Char dst ix '\xFFFD')
              else go =<< P.effect (encodeUtf8Char dst ix (w16ToChar w))
          _ -> P.fail BadEscapeSequence
        '"'# -> do
          str <- P.effect
            (PM.unsafeFreezeByteArray =<< PM.resizeMutableByteArray dst ix)
          pure (String (TS.fromShortByteStringUnsafe (byteArrayToShortByteString str)))
        _ -> go =<< P.effect (encodeUtf8Char dst ix (C# c))
  go 0

encodeUtf8Char :: MutableByteArray s -> Int -> Char -> ST s Int
encodeUtf8Char !marr !ix !c
  | c < '\128' = do
      PM.writeByteArray marr ix (c2w c)
      pure (ix + 1)
  | c < '\x0800' = do
      PM.writeByteArray marr ix
        (fromIntegral @Int @Word8 (unsafeShiftR (ord c) 6 .|. 0b11000000))
      PM.writeByteArray marr (ix + 1)
        (0b10000000 .|. (0b00111111 .&. (fromIntegral @Int @Word8 (ord c))))
      pure (ix + 2)
  | c <= '\xffff' = do
      PM.writeByteArray marr ix
        (fromIntegral @Int @Word8 (unsafeShiftR (ord c) 12 .|. 0b11100000))
      PM.writeByteArray marr (ix + 1)
        (0b10000000 .|. (0b00111111 .&. (fromIntegral @Int @Word8 (unsafeShiftR (ord c) 6))))
      PM.writeByteArray marr (ix + 2)
        (0b10000000 .|. (0b00111111 .&. (fromIntegral @Int @Word8 (ord c))))
      pure (ix + 3)
  | otherwise = error "encodeUtf8Char: write this"


-- Compute the maximum number of bytes that could possibly
-- be required to house the UTF-8 encoded string once any
-- JSON escape sequences have been resolved.
-- The correctness of this hinges on the assumption that
-- the UTF-8 encoding of a character never takes up more
-- bytes than its escape sequence.
string :: Int -> Parser JsonTokenizeException s Token
string !start = go 1 where
  go !canMemcpy = do
    P.any JsonTokenizeException >>= \case
      92 -> P.any JsonTokenizeException *> go 0 -- backslash
      34 -> do -- double quote
        !pos <- Unsafe.cursor
        case canMemcpy of
          1 -> do
            src <- Unsafe.expose
            str <- P.effect $ do
              let end = pos - 1
              let len = end - start
              dst <- PM.newByteArray len
              PM.copyByteArray dst 0 src start len
              PM.unsafeFreezeByteArray dst
            pure (String (TS.fromShortByteStringUnsafe (byteArrayToShortByteString str)))
          _ -> do
            Unsafe.unconsume (pos - start)
            let end = pos - 1
            let maxLen = end - start
            copyAndEscape maxLen
      W8# w -> go (canMemcpy .&. I# (ltWord# w 128##) .&. I# (gtWord# w 31##))

unI64 :: Int64 -> Int#
unI64 (I64# i) = i

unI :: Int -> Int#
unI (I# i) = i

byteArrayToShortByteString :: ByteArray -> BSS.ShortByteString
byteArrayToShortByteString (PM.ByteArray x) = BSS.SBS x

c2w :: Char -> Word8
c2w = fromIntegral . ord

-- Precondition: Not in the range [U+D800 .. U+DFFF]
w16ToChar :: Word16 -> Char
w16ToChar (W16# w) = C# (chr# (word2Int# w))
