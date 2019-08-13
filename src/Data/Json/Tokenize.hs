{-# language RankNTypes #-}
{-# language LambdaCase #-}
{-# language PolyKinds #-}
{-# language BangPatterns #-}
{-# language UnboxedTuples #-}
{-# language MagicHash #-}
{-# language DuplicateRecordFields #-}
{-# language DerivingStrategies #-}
{-# language DeriveFunctor #-}
{-# language StandaloneDeriving #-}

module Data.Json.Tokenize
  ( Token(..)
  , SmallNumber(..)
  , LargeNumber(..)
  ) where

import Data.Int (Int64)
import Data.Text.Short (ShortText)
import GHC.Exts (TYPE,Int#,State#,ByteArray#,SmallMutableArray#)
import Data.Builder (Builder(..))
import Data.Bytes.Parser (Parser(..))
import Data.Primitive.ByteArray (ByteArray)
import Data.Chunks (Chunks(ChunksCons,ChunksNil))
import Data.Bytes.Types (Bytes(..))
import GHC.Exts (Int(I#))

import qualified Data.Chunks as C
import qualified Control.Monad
import qualified Data.ByteString.Short.Internal as BSS
import qualified Data.Bytes.Parser as P
import qualified Data.Builder as B
import qualified Data.Primitive as PM
import qualified Data.Text.Short.Unsafe as TS
import qualified GHC.Exts as Exts

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

data JsonTokenizeException = JsonTokenizeException

data SmallNumber = SmallNumber
  { numerator :: !Int64
  , denominator :: !Int64
  , exponent :: !Int64
  }

data LargeNumber = LargeNumber
  { numerator :: !Integer
  , denominator :: !Integer
  , exponent :: !Integer
  }

run :: P Exts.RealWorld () -> Bytes -> Either JsonTokenizeException (Chunks Token)
run (P f) !bs = case Exts.runRW#
  -- The initial size of 16 elements is chosen somewhat
  -- arbitrarily. It is more than enough to saturate a
  -- cache line.
  (\s0 -> case Exts.newSmallArray# 16# errorThunk s0 of
    (# s1, marr0 #) -> case f marr0 0# 16# ChunksNil (unboxBytes bs) s1 of
      (# s2, r #) -> case r of
        (# e | #) -> (# s2, Left e #)
        (# | (# _, marr, off, _, ts, _, blen #) #) ->
          -- Recall that freezeSmallArray copies a slice.
          -- If resize functions ever become available for
          -- SmallArray, we should use that instead.
          case Exts.freezeSmallArray# marr 0# off s2 of
            (# s3, arr #) ->
              let !r = C.reverseOnto
                    (ChunksCons (PM.SmallArray arr) ChunksNil)
                    ts
               in (# s3, Right r #)
  ) of (# _, cs #) -> cs

errorThunk :: a
{-# noinline errorThunk #-}
errorThunk = error "array-builder:Data.Builder: error"

manyTokens :: P s ()
manyTokens = do
  t <- liftParser oneToken
  liftBuilder (B.singleton t)
  done <- liftParser $ do
    P.skipAscii ' '
    P.isEndOfInput
  if done
    then pure ()
    else manyTokens

oneToken :: P.Parser JsonTokenizeException s Token
oneToken = P.anyAscii JsonTokenizeException >>= \case
  '{' -> pure LeftBrace
  '}' -> pure RightBrace
  '[' -> pure LeftBracket
  ']' -> pure RightBracket
  ',' -> pure Comma
  ':' -> pure Colon
  't' -> do
    P.ascii JsonTokenizeException 'r'
    P.ascii JsonTokenizeException 'u'
    P.ascii JsonTokenizeException 'e'
    pure BooleanTrue
  'f' -> do
    P.ascii JsonTokenizeException 'a'
    P.ascii JsonTokenizeException 'l'
    P.ascii JsonTokenizeException 's'
    P.ascii JsonTokenizeException 'e'
    pure BooleanFalse
  'n' -> do
    P.ascii JsonTokenizeException 'u'
    P.ascii JsonTokenizeException 'l'
    P.ascii JsonTokenizeException 'l'
    pure Null
  '"' -> do
    start <- P.cursor
    -- TODO: Use a UTF-8 skip function instead.
    P.skipUntilAsciiConsume JsonTokenizeException '"'
    end <- P.cursor
    let off = start
        len = (end - start) - 1
    arr <- P.expose
    str <- P.effect $ do
      buf <- PM.newByteArray len
      PM.copyByteArray buf 0 arr off len
      PM.unsafeFreezeByteArray buf
    pure (String (TS.fromShortByteStringUnsafe (byteArrayToShortByteString str)))
  c -> P.failure JsonTokenizeException

liftParser :: Parser JsonTokenizeException s a -> P s a
liftParser (Parser g) = P
  (\marr off len cs b s0 -> case g b s0 of
    (# s1, r #) -> case r of
      (# e | #) -> (# s1, (# e | #) #)
      (# | (# a, soff, slen #) #) ->
        (# s1, (# | (# a, marr, off, len, cs, soff, slen #) #) #)
  )

liftBuilder :: Builder Token -> P s ()
{-# inline liftBuilder #-}
liftBuilder (Builder f) = P
  (\marr0 off0 len0 cs0 (# _, soff, slen #) s0 -> case f marr0 off0 len0 cs0 s0 of
    (# s1, marr1, off1, len1, cs1 #) ->
      (# s1, (# | (# (), marr1, off1, len1, cs1, soff, slen #) #) #)
  )

byteArrayToShortByteString :: ByteArray -> BSS.ShortByteString
byteArrayToShortByteString (PM.ByteArray x) = BSS.SBS x

newtype P s a = P
  { unP ::
      SmallMutableArray# s Token -> Int# -> Int# -> Chunks Token
   -> Bytes#
   -> State# s
   -> (# State# s
      ,  Result# JsonTokenizeException s a #)
  }

instance Functor (P s) where
  fmap f (P g) = P
    (\marr0 off0 len0 cs0 b s0 -> case g marr0 off0 len0 cs0 b s0 of
      (# s1, r #) -> case r of
        (# e | #) -> (# s1, (# e | #) #)
        (# | (# a, marr1, off1, len1, cs1, soff, slen #) #) ->
          (# s1, (# | (# f a, marr1, off1, len1, cs1, soff, slen #) #) #)
    )

instance Applicative (P s) where
  pure x = P 
    (\marr0 off0 len0 cs0 (# _, soff, slen #) s0 -> 
      (# s0, (# | (# x, marr0, off0, len0, cs0, soff, slen #) #) #)
    )
  (<*>) = Control.Monad.ap

instance Monad (P s) where
  P f >>= g = P
    (\marr0 off0 len0 cs0 b@(# barr,_,_ #) s0 -> case f marr0 off0 len0 cs0 b s0 of
      (# s1, r #) -> case r of
        (# e | #) -> (# s1, (# e | #) #)
        (# | (# a, marr1, off1, len1, cs1, soff, slen #) #) ->
          unP (g a) marr1 off1 len1 cs1 (# barr, soff, slen #) s1
    )

type Bytes# = (# ByteArray#, Int#, Int# #)
type Result# e s (a :: TYPE r) =
  (# e
  | (# a, SmallMutableArray# s Token, Int#, Int#, Chunks Token, Int#, Int# #) #) -- ints are offset and length

unboxBytes :: Bytes -> Bytes#
unboxBytes (Bytes (PM.ByteArray a) (I# b) (I# c)) =
  (# a, b, c #)
