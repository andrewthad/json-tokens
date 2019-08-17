{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}

import Data.Bytes.Types (Bytes(..))
import Data.Char (ord)
import Data.Json.Tokenize (decode)
import Data.Primitive (ByteArray)
import Data.Word (Word8)
import Gauge.Main (defaultMain,bgroup,bench,whnf)

import Twitter1 (encodedTwitter1)
import Twitter10 (encodedTwitter10)
import Twitter100 (encodedTwitter100)

import qualified Data.Primitive as PM
import qualified GHC.Exts as Exts

main :: IO ()
main = defaultMain
  [ bench "trivial" $ whnf decode trivial
  , bench "geojson" $ whnf decode geojson
  , bgroup "twitter"
    [ bench "1" (whnf decode (toSlice encodedTwitter1))
    , bench "10" (whnf decode (toSlice encodedTwitter10))
    , bench "100" (whnf decode (toSlice encodedTwitter100))
    ]
  ]

trivial :: Bytes
trivial = pack $ concat
  [ "{\"handler\":\"bar\""
  , ",\"identifier\":2534"
  , ",\"alive\":true"
  , "}"
  ]

geojson :: Bytes
geojson = pack $ concat
  [ "{"
  , "\"as\": \"AS16509 Amazon.com, Inc.\","
  , "\"city\": \"Boardman\","
  , "\"country\": \"United States\","
  , "\"countryCode\": \"US\","
  , "\"isp\": \"Amazon\","
  , "\"lat\": 45.8696,"
  , "\"lon\": -119.688,"
  , "\"org\": \"Amazon\","
  , "\"query\": \"54.148.84.95\","
  , "\"region\": \"OR\","
  , "\"regionName\": \"Oregon\","
  , "\"status\": \"success\","
  , "\"timezone\": \"America\\/Los_Angeles\","
  , "\"zip\": \"97818\""
  , "}"
  ]

pack :: String -> Bytes
pack str =
  let barr = Exts.fromList (map (fromIntegral @Int @Word8 . ord) str)
   in Bytes barr 0 (PM.sizeofByteArray barr)

toSlice :: ByteArray -> Bytes
toSlice b = Bytes b 0 (PM.sizeofByteArray b)

