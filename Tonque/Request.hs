module Tonque.Request where

import           Codec.Binary.UTF8.String
import           Codec.Text.IConv
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.ByteString.Lazy.UTF8 as U
import           Data.Text (Text)
import qualified Data.Text as T
import           Network.HTTP

import Tonque.Type

request :: Tonque.Type.URL -> IO Text
request url = do
  res <- simpleHTTP $ getRequest $ T.unpack url
  str <- getResponseBody res
  let bstr = BSLC.pack str
      ustr = U.toString $ convert "SJIS" "UTF-8" bstr
  return $ T.pack str
