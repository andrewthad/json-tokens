{-# language BangPatterns #-}
{-# language DuplicateRecordFields #-}
{-# language NamedFieldPuns #-}
{-# language TypeApplications #-}

module Data.Json.Number
  ( toWord16
  ) where

import Prelude hiding (exponent)
import Data.Int (Int64)
import Data.Word (Word16)
import Data.Json.Tokenize (Token(..),SmallNumber(..),LargeNumber(..))

toWord16 :: Token -> Maybe Word16
toWord16 (NumberSmall (SmallNumber{coefficient,exponent}))
  | exponent >= 0, exponent < 5, coefficient >= 0, coefficient < 65536
  , r <- exp10 coefficient exponent
  , y <- fromIntegral @Int64 @Word16 r
  , fromIntegral @Word16 @Int64 y == r
    = Just y
  | otherwise = Nothing
toWord16 (NumberLarge (LargeNumber{coefficient,exponent}))
  | exponent >= 0, exponent < 5, coefficient >= 0, coefficient < 65536
  , r <- coefficient * (10 ^ exponent)
  , r < 65536
  , y <- fromIntegral @Integer @Word16 r
    = Just y
  | otherwise = Nothing
toWord16 _ = Nothing

-- Precondition: the exponent is greater than zero
exp10 :: Int64 -> Int64 -> Int64
exp10 !a !e = case e of
  0 -> a
  _ -> exp10 (a * 10) (e - 1)
