module Tonque.Request where

import           Codec.Binary.UTF8.String
import           Codec.Text.IConv
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.Text (Text)
import qualified Data.Text as T
import           Network.HTTP
import           Network.URI

import Tonque.Type

request :: Tonque.Type.URL -> IO Text
request url = do
   case uriM of
       Just uri -> do
           res <- simpleHTTP $ Request uri GET [] BSL.empty
           bstr <- getResponseBody res
           let ustr = decodeString $ BSLC.unpack $ convert "CP932" "UTF-8" bstr
           return $ T.pack ustr
       Nothing -> error $ "Invalid URL: " ++ T.unpack url
  where
    uriM = parseURI $ T.unpack url
