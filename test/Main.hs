{-# language OverloadedStrings #-}
{-# language TypeApplications #-}

import Data.Bytes.Types (Bytes(..))
import Data.Char (ord)
import Data.Either (isRight,isLeft)
import Data.Primitive (ByteArray)
import Data.Word (Word8)
import Test.Tasty (defaultMain,testGroup,TestTree)
import Test.Tasty.HUnit ((@=?))

import Twitter1 (encodedTwitter1,tokensTwitter1)
import Twitter10 (encodedTwitter10)
import Twitter100 (encodedTwitter100)

import qualified Data.Foldable as F
import qualified Data.Json.Tokenize as J
import qualified Data.Number.Scientific as SCI
import qualified Data.Primitive as PM
import qualified GHC.Exts as Exts
import qualified Test.Tasty.HUnit as THU

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ THU.testCase "A" $
      Right [J.LeftBrace,J.RightBrace]
      @=?
      fmap F.toList (J.decode (pack "{}"))
  , THU.testCase "B" $
      Right [J.LeftBrace,J.String "foo",J.Colon,J.String "bar",J.RightBrace]
      @=?
      fmap F.toList (J.decode (pack "{\"foo\" :\"bar\"}"))
  , THU.testCase "C" $
      Right [J.BooleanTrue]
      @=?
      fmap F.toList (J.decode (pack " true  "))
  , THU.testCase "D" $
      Right
        [ J.LeftBracket
        , J.Number (SCI.small (-42) 0)
        , J.RightBracket
        ]
      @=?
      fmap F.toList (J.decode (pack "[-42]"))
  , THU.testCase "E" $
      Right
        [ J.LeftBracket
        , J.Number (SCI.small (-2) 5)
        , J.Comma
        , J.Number (SCI.small (-13) (-2))
        , J.RightBracket
        ]
      @=?
      fmap F.toList (J.decode (pack "[-2e5,-13E-2]"))
  , THU.testCase "F" $
      Right
        [ J.LeftBracket
        , J.Number (SCI.small 2 5)
        , J.Comma
        , J.Number (SCI.small 13 (-2))
        , J.RightBracket
        ]
      @=?
      fmap F.toList (J.decode (pack "[2e+5,13E-2]"))
  , THU.testCase "F" $
      Right
        [ J.LeftBracket
        , J.Number (SCI.small (-20) 4)
        , J.Comma
        , J.Number (SCI.small 136 (-1))
        , J.RightBracket
        ]
      @=?
      fmap F.toList (J.decode (pack "[-2.0e5,13.6E+0]"))
  , THU.testCase "G" $
      Right [ J.Number (SCI.large (6553565535) (-5)) ]
      @=?
      fmap F.toList (J.decode (pack "65535.65535"))
  , THU.testCase "H" $
      Right [ J.Number (SCI.large (65535001) (-3)) ]
      @=?
      fmap F.toList (J.decode (pack "65535.001"))
  , THU.testCase "I" $
      Right [ J.Number (SCI.large 9223372036854775808 0) ]
      @=?
      fmap F.toList (J.decode (pack "9223372036854775808"))
  , THU.testCase "J" $
      Right
        [ J.LeftBracket
        , J.Number (SCI.large 9223372036854775818 0)
        , J.Comma
        , J.Number (SCI.large (-21) (-9223372036854775820))
        , J.RightBracket
        ]
      @=?
      fmap F.toList
        ( J.decode
          (pack "[9223372036854775818,-2.1E-9223372036854775819]")
        )
  , THU.testCase "K" $
      Right tokensTwitter1
      @=?
      fmap F.toList (J.decode (toSlice encodedTwitter1))
  , THU.testCase "L" $
      isRight (J.decode (toSlice encodedTwitter10)) @=? True
  , THU.testCase "M" $
      isRight (J.decode (toSlice encodedTwitter100)) @=? True
  , THU.testCase "N" $
      isLeft (J.decode (pack " 00")) @=? True
  , THU.testCase "O" $
      isLeft (J.decode (pack " 05")) @=? True
  ]

pack :: String -> Bytes
pack str =
  let barr = Exts.fromList (map (fromIntegral @Int @Word8 . ord) str)
   in Bytes barr 0 (PM.sizeofByteArray barr)

toSlice :: ByteArray -> Bytes
toSlice b = Bytes b 0 (PM.sizeofByteArray b)

