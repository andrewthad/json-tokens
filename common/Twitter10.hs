{-# LANGUAGE QuasiQuotes #-}
module Twitter10
  ( encodedTwitter10
  ) where

import Data.ByteString.Short (ShortByteString,toShort)
import Data.Primitive (ByteArray)
import Data.Text.Encoding (encodeUtf8)
import NeatInterpolation (text)

import qualified Data.Primitive as PM
import qualified Data.ByteString.Short.Internal as BSS

shortByteStringToByteArray :: ShortByteString -> ByteArray 
shortByteStringToByteArray (BSS.SBS x) = PM.ByteArray x

encodedTwitter10 :: ByteArray
encodedTwitter10 = shortByteStringToByteArray $ toShort $ encodeUtf8
  [text|
    {
        "completed_in": 0.012714,
        "max_id": 30120402839666689,
        "max_id_str": "30120402839666689",
        "next_page": "?page=2&max_id=30120402839666689&rpp=10&q=haskell",
        "page": 1,
        "query": "haskell",
        "refresh_url": "?since_id=30120402839666689&q=haskell",
        "results": [
            {
                "created_at": "Wed, 26 Jan 2011 04:30:38 +0000",
                "from_user": "pboudarga",
                "from_user_id": 207858021,
                "from_user_id_str": "207858021",
                "geo": null,
                "id": 30120402839666689,
                "id_str": "30120402839666689",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a3.twimg.com/sticky/default_profile_images/default_profile_2_normal.png",
                "source": "&lt;a href=&quot;http://foursquare.com&quot; rel=&quot;nofollow&quot;&gt;foursquare&lt;/a&gt;",
                "text": "I'm at Rolla Sushi Grill (27737 Bouquet Canyon Road, #106, Btw Haskell Canyon and Rosedell Drive, Saugus) http://4sq.com/gqqdhs",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Wed, 26 Jan 2011 04:25:23 +0000",
                "from_user": "YNK33",
                "from_user_id": 69988683,
                "from_user_id_str": "69988683",
                "geo": null,
                "id": 30119083059978240,
                "id_str": "30119083059978240",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a0.twimg.com/profile_images/1211955817/avatar_7888_normal.gif",
                "source": "&lt;a href=&quot;http://twitterfeed.com&quot; rel=&quot;nofollow&quot;&gt;twitterfeed&lt;/a&gt;",
                "text": "hsndfile 0.5.0: Free and open source Haskell bindings for libsndfile http://bit.ly/gHaBWG Mac Os",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Wed, 26 Jan 2011 04:24:28 +0000",
                "from_user": "satzz",
                "from_user_id": 81492,
                "from_user_id_str": "81492",
                "geo": null,
                "id": 30118851488251904,
                "id_str": "30118851488251904",
                "iso_language_code": "ja",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a1.twimg.com/profile_images/423894208/Picture_7_normal.jpg",
                "source": "&lt;a href=&quot;http://www.hootsuite.com&quot; rel=&quot;nofollow&quot;&gt;HootSuite&lt;/a&gt;",
                "text": "Emacs\u306e\u30e2\u30fc\u30c9\u8868\u793a\u304c\u4eca(Ruby Controller Outputz RoR Flymake REl hs)\u3068\u306a\u3063\u3066\u3066\u3088\u304f\u308f\u304b\u3089\u306a\u3044\u3093\u3060\u3051\u3069\u6700\u5f8c\u306eREl\u3068\u304bhs\u3063\u3066\u4f55\u3060\u308d\u3046\u2026haskell\u3068\u304b2\u5e74\u4ee5\u4e0a\u66f8\u3044\u3066\u306a\u3044\u3051\u3069\u2026",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Wed, 26 Jan 2011 04:19:19 +0000",
                "from_user": "planet_ocaml",
                "from_user_id": 9518356,
                "from_user_id_str": "9518356",
                "geo": null,
                "id": 30117557788741632,
                "id_str": "30117557788741632",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a2.twimg.com/profile_images/119165723/ocaml-icon_normal.png",
                "source": "&lt;a href=&quot;http://twitterfeed.com&quot; rel=&quot;nofollow&quot;&gt;twitterfeed&lt;/a&gt;",
                "text": "I so miss #haskell type classes in #ocaml - i want to do something like refinement. Also why does ocaml not have... http://bit.ly/geYRwt",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Wed, 26 Jan 2011 04:16:32 +0000",
                "from_user": "aprikip",
                "from_user_id": 218059,
                "from_user_id_str": "218059",
                "geo": null,
                "id": 30116854940835840,
                "id_str": "30116854940835840",
                "iso_language_code": "ja",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a1.twimg.com/profile_images/1053837723/twitter-icon9_normal.jpg",
                "source": "&lt;a href=&quot;http://sites.google.com/site/yorufukurou/&quot; rel=&quot;nofollow&quot;&gt;YoruFukurou&lt;/a&gt;",
                "text": "yatex-mode\u3084haskell-mode\u306e\u3053\u3068\u3067\u3059\u306d\u3001\u308f\u304b\u308a\u307e\u3059\u3002",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Wed, 26 Jan 2011 04:15:30 +0000",
                "from_user": "dysinger",
                "from_user_id": 216363,
                "from_user_id_str": "216363",
                "geo": null,
                "id": 30116594684264448,
                "id_str": "30116594684264448",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a1.twimg.com/profile_images/72454310/Tim-Avatar_normal.png",
                "source": "&lt;a href=&quot;http://www.nambu.com/&quot; rel=&quot;nofollow&quot;&gt;Nambu&lt;/a&gt;",
                "text": "Haskell in Hawaii tonight for me... #fun",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Wed, 26 Jan 2011 04:13:36 +0000",
                "from_user": "DanMil",
                "from_user_id": 1774820,
                "from_user_id_str": "1774820",
                "geo": null,
                "id": 30116117682851840,
                "id_str": "30116117682851840",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a2.twimg.com/profile_images/61169291/dan_desert_thumb_normal.jpg",
                "source": "&lt;a href=&quot;http://twitter.com/&quot;&gt;web&lt;/a&gt;",
                "text": "@ojrac @chewedwire @tomheon Haskell isn't a language, it's a belief system.  A seductive one...",
                "to_user": "ojrac",
                "to_user_id": 1594784,
                "to_user_id_str": "1594784"
            },
            {
                "created_at": "Wed, 26 Jan 2011 04:11:06 +0000",
                "from_user": "djspiewak",
                "from_user_id": 659256,
                "from_user_id_str": "659256",
                "geo": null,
                "id": 30115488931520512,
                "id_str": "30115488931520512",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a0.twimg.com/profile_images/746976711/angular-final_normal.jpg",
                "source": "&lt;a href=&quot;http://itunes.apple.com/us/app/twitter/id409789998?mt=12&quot; rel=&quot;nofollow&quot;&gt;Twitter for Mac&lt;/a&gt;",
                "text": "One of the very nice things about Haskell as opposed to SML is the reduced proliferation of identifiers (e.g. andb, orb, etc). #typeclasses",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Wed, 26 Jan 2011 04:06:12 +0000",
                "from_user": "listwarenet",
                "from_user_id": 144546280,
                "from_user_id_str": "144546280",
                "geo": null,
                "id": 30114255890026496,
                "id_str": "30114255890026496",
                "iso_language_code": "no",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a1.twimg.com/a/1295051201/images/default_profile_1_normal.png",
                "source": "&lt;a href=&quot;http://1e10.org/cloud/&quot; rel=&quot;nofollow&quot;&gt;1e10&lt;/a&gt;",
                "text": "http://www.listware.net/201101/haskell-cafe/84752-re-haskell-cafe-gpl-license-of-h-matrix-and-prelude-numeric.html Re: Haskell-c",
                "to_user_id": null,
                "to_user_id_str": null
            },
            {
                "created_at": "Wed, 26 Jan 2011 04:01:29 +0000",
                "from_user": "ojrac",
                "from_user_id": 1594784,
                "from_user_id_str": "1594784",
                "geo": null,
                "id": 30113067333324800,
                "id_str": "30113067333324800",
                "iso_language_code": "en",
                "metadata": {
                    "result_type": "recent"
                },
                "profile_image_url": "http://a2.twimg.com/profile_images/378515773/square-profile_normal.jpg",
                "source": "&lt;a href=&quot;http://twitter.com/&quot;&gt;web&lt;/a&gt;",
                "text": "RT @tomheon: @ojrac @chewedwire Don't worry, learning Haskell will not give you any clear idea what monad means.",
                "to_user_id": null,
                "to_user_id_str": null
            }
        ],
        "results_per_page": 10,
        "since_id": 0,
        "since_id_str": "0"
    }
  |]
