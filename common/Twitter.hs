module Twitter
  ( Response(..)
  , Result(..)
  , decodeResponse
  ) where

import Data.Json.Tokenize (SmallNumber,Token(..),decode)
import Data.Parser (parse)

data Response = Response
  { completed_in :: {-# UNPACK #-} !SmallNumber
  , max_id :: !Word64
  , max_id_str :: !ShortText
  , next_page :: !ShortText
  , page :: !Word64
  , query :: !ShortText
  , refresh_url :: !ShortText
  , results :: !(SmallArray Result)
  }

data Result = Result
  { created_at :: !ShortText
  , from_user :: !ShortText
  , from_user_id :: !Word64
  , from_user_id_str :: !ShortText
  , id :: !Word64
  , id_str :: !ShortText
  , iso_language_code :: !ShortText
  , profile_image_url :: !ShortText
  , source :: !ShortText
  , text :: !ShortText
  }

data ResponseException = ResponseException

decodeResponse ::
     Bytes
  -> Either (Either JsonTokenizeException ResponseException) Response
decodeResponse bs = case Tokenize.decode bs of
  Left err -> Left (Left err)
  Right tokens -> case parse responseParser tokens of
    Left err -> Left (Right err)
    Right r -> Right r

responseParser :: Parser Token ResponseException s Response
responseParser = do
  
