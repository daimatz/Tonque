module Tonque.Util where

import           Codec.Binary.UTF8.String
import           Codec.Text.IConv (convertFuzzy, Fuzzy(..))
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.Convertible (safeConvert)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (UTCTime)
import           Data.Time.Format (formatTime)
import           Network.HTTP
import           Network.URI (parseURI)
import           System.Locale (defaultTimeLocale)

import           Tonque.Type

textShow :: Show a => a -> Text
textShow = T.pack . show

epochToUTC :: EpochTime -> UTCTime
epochToUTC t = case safeConvert t of
    Left err  -> error $ show err
    Right res -> res

timeFormat :: UTCTime -> Text
timeFormat = T.pack . formatTime defaultTimeLocale "%F %T"

request :: URL -> IO Text
request url = do
   case uriM of
       Just uri -> do
           res <- simpleHTTP $ Request uri GET [] BSL.empty
           getResponseBody res >>= return . toUTF8
       Nothing -> error $ "Invalid URL: " ++ T.unpack url
  where
    uriM = parseURI $ T.unpack url

toUTF8 :: BSL.ByteString -> Text
toUTF8 = T.pack
       . decodeString
       . BSLC.unpack
       . convertFuzzy Discard "SJIS" "UTF-8"

toSJIS :: Text -> BSL.ByteString
toSJIS = convertFuzzy Discard "UTF-8" "SJIS"
       . BSLC.pack
       . encodeString
       . T.unpack
