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
  , SmallNumber(..)
  , LargeNumber(..)
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

import qualified Data.ByteString.Short.Internal as BSS
import qualified Data.Bytes.Parser as P
import qualified Data.Builder.ST as B
import qualified Data.Chunks as C
import qualified Data.Primitive as PM
import qualified Data.Text.Short.Unsafe as TS

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
  | NumberSmall {-# UNPACK #-} !SmallNumber
  | NumberLarge {-# UNPACK #-} !LargeNumber
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
    P.Success cs _ _ -> pure (Right cs)

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
oneToken = P.anyAscii JsonTokenizeException >>= \case
  '{' -> pure LeftBrace
  '}' -> pure RightBrace
  '[' -> pure LeftBracket
  ']' -> pure RightBracket
  ',' -> pure Comma
  ':' -> pure Colon
  't' -> do
    P.ascii3 NonTrueCharacter 'r' 'u' 'e'
    pure BooleanTrue
  'f' -> do
    P.ascii4 NonFalseCharacter 'a' 'l' 's' 'e'
    pure BooleanFalse
  'n' -> do
    P.ascii3 NonNumericCharacter 'u' 'l' 'l'
    pure Null
  '"' -> do
    start <- P.cursor
    string start
  '-' -> number (-1) `P.orElse` largeNumber (-1)
  c | c >= '0' && c <= '9' -> do
        P.unconsume 1
        number 1 `P.orElse` largeNumber 1
  _ -> P.fail NonLeadingCharacter

copyAndEscape :: Int -> Parser JsonTokenizeException s Token
copyAndEscape !maxLen = do
  !dst <- P.effect (PM.newByteArray maxLen)
  let go !ix = P.anyUtf8# JsonTokenizeException `P.bindChar` \c -> case c of
        '\\'# -> P.anyAscii JsonTokenizeException >>= \case
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
            w <- P.hexWord16 JsonTokenizeException
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
        !pos <- P.cursor
        case canMemcpy of
          1 -> do
            src <- P.expose
            str <- P.effect $ do
              let end = pos - 1
              let len = end - start
              dst <- PM.newByteArray len
              PM.copyByteArray dst 0 src start len
              PM.unsafeFreezeByteArray dst
            pure (String (TS.fromShortByteStringUnsafe (byteArrayToShortByteString str)))
          _ -> do
            P.unconsume (pos - start)
            let end = pos - 1
            let maxLen = end - start
            copyAndEscape maxLen
      W8# w -> go (canMemcpy .&. I# (ltWord# w 128##) .&. I# (gtWord# w 31##))

number :: Int64 -> Parser JsonTokenizeException s Token
number !sign = do
  coeff <- P.decWord32 JsonTokenizeException
  P.isEndOfInput >>= \case
    True -> pure (NumberSmall (SmallNumber (sign * (fromIntegral coeff)) 0))
    False -> P.anyAscii# JsonTokenizeException `P.bindChar` \c -> case c of
      '.'# -> do
        !start <- P.cursor
        !numer <- P.decWord16 JsonTokenizeException
        !end <- P.cursor
        let !logDenom = fromIntegral @Int @Int64 (end - start)
            !coeff' = fromIntegral @Word32 @Int64 coeff * (10 ^ logDenom) + fromIntegral @Word16 @Int64 numer
        P.isEndOfInput >>= \case
          True -> pure (NumberSmall (SmallNumber (sign * coeff') (negate logDenom)))
          False -> P.anyAscii# JsonTokenizeException `P.bindChar` \x ->
            attemptSmallExp x (unI64 sign) (unI64 coeff') (unI64 (negate logDenom))
      _ -> attemptSmallExp c (unI64 sign) (unI64 (fromIntegral coeff)) (unI64 0)

largeNumber :: Int -> Parser JsonTokenizeException s Token
largeNumber !sign = do
  coeff <- P.decPositiveInteger JsonTokenizeException
  P.isEndOfInput >>= \case
    True -> pure (NumberLarge (LargeNumber (fromIntegral sign * coeff) 0))
    False -> P.anyAscii# JsonTokenizeException `P.bindChar` \c -> case c of
      '.'# -> do
        !start <- P.cursor
        !numer <- P.decPositiveInteger JsonTokenizeException
        !end <- P.cursor
        let !logDenom = end - start
            !coeff' = coeff * (10 ^ fromIntegral @Int @Integer logDenom) + numer
        P.isEndOfInput >>= \case
          True -> pure (NumberLarge (LargeNumber (fromIntegral sign * coeff') (fromIntegral (negate logDenom))))
          False -> P.anyAscii# JsonTokenizeException `P.bindChar` \x ->
            attemptLargeExp x (unI sign) coeff' (unI (negate logDenom))
      _ -> attemptLargeExp c (unI sign) coeff 0#

unI64 :: Int64 -> Int#
unI64 (I64# i) = i

unI :: Int -> Int#
unI (I# i) = i

attemptSmallExp :: Char# -> Int# -> Int# -> Int# -> Parser JsonTokenizeException s Token
attemptSmallExp !c# !sign# !coeff# !deltaExp# = case c# of
  'e'# -> do
    e <- afterExp
    pure (NumberSmall (SmallNumber (sign * coeff) (e + deltaExp)))
  'E'# -> do
    e <- afterExp
    pure (NumberSmall (SmallNumber (sign * coeff) (e + deltaExp)))
  _ -> do
    P.unconsume 1
    pure (NumberSmall (SmallNumber (sign * coeff) deltaExp))
  where
  sign = I64# sign#
  coeff = I64# coeff#
  deltaExp = I64# deltaExp#

attemptLargeExp :: Char# -> Int# -> Integer -> Int# -> Parser JsonTokenizeException s Token
attemptLargeExp !c# !sign# !coeff !deltaExp# = case c# of
  'e'# -> do
    e <- afterExpLarge
    pure (NumberLarge (LargeNumber (sign * coeff) (e + fromIntegral deltaExp)))
  'E'# -> do
    e <- afterExpLarge
    pure (NumberLarge (LargeNumber (sign * coeff) (e + fromIntegral deltaExp)))
  _ -> do
    P.unconsume 1
    pure (NumberLarge (LargeNumber (sign * coeff) (fromIntegral deltaExp)))
  where
  sign = fromIntegral (I# sign#) :: Integer
  deltaExp = I# deltaExp#

afterExp :: Parser JsonTokenizeException s Int64
afterExp = P.anyAscii JsonTokenizeException >>= \case
  '+' -> fmap fromIntegral (P.decWord16 NonNumericCharacter)
  '-' -> fmap
    (\x -> negate (fromIntegral x))
    (P.decWord16 JsonTokenizeException)
  _ -> do
    P.unconsume 1
    fmap fromIntegral (P.decWord16 NonNumericCharacter)

afterExpLarge :: Parser JsonTokenizeException s Integer
afterExpLarge = P.anyAscii JsonTokenizeException >>= \case
  '+' -> P.decPositiveInteger NonNumericCharacter
  '-' -> fmap negate (P.decPositiveInteger NonNumericCharacter)
  _ -> do
    P.unconsume 1
    P.decPositiveInteger NonNumericCharacter

byteArrayToShortByteString :: ByteArray -> BSS.ShortByteString
byteArrayToShortByteString (PM.ByteArray x) = BSS.SBS x

c2w :: Char -> Word8
c2w = fromIntegral . ord

-- Precondition: Not in the range [U+D800 .. U+DFFF]
w16ToChar :: Word16 -> Char
w16ToChar (W16# w) = C# (chr# (word2Int# w))
