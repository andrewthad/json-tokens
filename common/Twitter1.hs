{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Twitter1
  ( encodedTwitter1
  , tokensTwitter1
  ) where

import Data.ByteString.Short (ShortByteString,toShort)
import Data.Json.Tokenize (Token(..))
import Data.Primitive (ByteArray)
import Data.Text.Encoding (encodeUtf8)
import NeatInterpolation (text)

import qualified Data.Primitive as PM
import qualified Data.ByteString.Short.Internal as BSS
import qualified Data.Number.Scientific as SCI

shortByteStringToByteArray :: ShortByteString -> ByteArray 
shortByteStringToByteArray (BSS.SBS x) = PM.ByteArray x

encodedTwitter1 :: ByteArray
encodedTwitter1 = shortByteStringToByteArray $ toShort $ encodeUtf8
  [text|
    {
        "completed_in": 0.012606,
        "max_id": 30159761706061824,
        "max_id_str": "30159761706061824",
        "next_page": "?page=2&max_id=30159761706061824&rpp=1&q=haskell",
        "page": 1,
        "query": "haskell",
        "refresh_url": "?since_id=30159761706061824&q=haskell",
        "results": [
            {
                "created_at": "Wed, 26 Jan 2011 07:07:02 +0000",
                "from_user": "kazu_yamamoto",
                "from_user_id": 80430860,
                "from_user_id_str": "80430860",
                "geo": null,
                "id": 30159761706061824,
                "id_str": "30159761706061824",
                "iso_language_code": "no",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a2.twimg.com/profile_images/536455139/icon32_normal.png",
                "source": "&lt;a href=&quot;http://twitter.com/&quot;&gt;web&lt;/a&gt;",
                "text": "Haskell Server Pages \u3063\u3066\u3001\u307e\u3060\u7d9a\u3044\u3066\u3044\u305f\u306e\u304b\uff01",
                "to_user_id": null,
                "to_user_id_str": null
            }
        ],
        "results_per_page": 1,
        "since_id": 0,
        "since_id_str": "0"
    }
  |]

tokensTwitter1 :: [Token]
tokensTwitter1 =
  [ LeftBrace
  , String "completed_in"
  , Colon
  , Number (SCI.small 12606 (-6))
  , Comma
  , String "max_id"
  , Colon
  , Number (SCI.large 30159761706061824 0)
  , Comma
  , String "max_id_str"
  , Colon
  , String "30159761706061824"
  , Comma
  , String "next_page"
  , Colon
  , String "?page=2&max_id=30159761706061824&rpp=1&q=haskell"
  , Comma
  , String "page"
  , Colon
  , Number (SCI.small 1 0)
  , Comma
  , String "query"
  , Colon
  , String "haskell"
  , Comma
  , String "refresh_url"
  , Colon
  , String "?since_id=30159761706061824&q=haskell"
  , Comma
  , String "results"
  , Colon
  , LeftBracket
  , LeftBrace
  , String "created_at"
  , Colon
  , String "Wed, 26 Jan 2011 07:07:02 +0000"
  , Comma
  , String "from_user"
  , Colon
  , String "kazu_yamamoto"
  , Comma
  , String "from_user_id"
  , Colon
  , Number (SCI.small 80430860 0)
  , Comma
  , String "from_user_id_str"
  , Colon
  , String "80430860"
  , Comma
  , String "geo"
  , Colon
  , Null
  , Comma
  , String "id"
  , Colon
  , Number (SCI.large 30159761706061824 0)
  , Comma
  , String "id_str"
  , Colon
  , String "30159761706061824"
  , Comma
  , String "iso_language_code"
  , Colon
  , String "no"
  , Comma
  , String "metadata"
  , Colon
  , LeftBrace
  , String "result_type"
  , Colon
  , String "recent"
  , RightBrace
  , Comma
  , String "profile_image_url"
  , Colon
  , String "http://a2.twimg.com/profile_images/536455139/icon32_normal.png"
  , Comma
  , String "source"
  , Colon
  , String "&lt;a href=&quot;http://twitter.com/&quot;&gt;web&lt;/a&gt;"
  , Comma
  , String "text"
  , Colon
  , String "Haskell Server Pages \12387\12390\12289\12414\12384\32154\12356\12390\12356\12383\12398\12363\65281"
  , Comma
  , String "to_user_id"
  , Colon
  , Null
  , Comma
  , String "to_user_id_str"
  , Colon
  , Null
  , RightBrace
  , RightBracket
  , Comma
  , String "results_per_page"
  , Colon
  , Number (SCI.small 1 0)
  , Comma
  , String "since_id"
  , Colon
  , Number (SCI.small 0 0)
  , Comma
  , String "since_id_str"
  , Colon
  , String "0"
  , RightBrace
  ]
