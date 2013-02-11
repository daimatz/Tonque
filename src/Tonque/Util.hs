module Tonque.Util where

import           Codec.Text.IConv        (Fuzzy (..), convertFuzzy)
import           Data.ByteString.Lazy    (ByteString)
import qualified Data.ByteString.Lazy    as BSL
import           Data.Convertible        (safeConvert)
import           Data.Monoid             ((<>))
import           Data.Text.Lazy          (Text)
import qualified Data.Text.Lazy          as TL
import           Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import           Data.Time               (UTCTime)
import           Data.Time.Format        (formatTime)
import qualified Network.HTTP            as H
import           Network.URI             (parseURI)
import           System.Locale           (defaultTimeLocale)

import           Tonque.Type

textShow :: Show a => a -> Text
textShow = TL.pack . show

epochToUTC :: EpochTime -> UTCTime
epochToUTC t = case safeConvert t of
    Left err  -> error $ show err
    Right res -> res

timeFormat :: UTCTime -> Text
timeFormat = TL.pack . formatTime defaultTimeLocale "%F %T"

request :: URL -> IO Text
request url = do
    flip (maybe $ error $ TL.unpack $ "Invalid URL: " <> url) uriM $ \uri -> do
         res <- H.simpleHTTP $ H.Request uri H.GET [] BSL.empty
         H.getResponseBody res >>= return . toUTF8
  where
    uriM = parseURI $ TL.unpack url

toUTF8 :: ByteString -> Text
toUTF8 = decodeUtf8 . convertFuzzy Discard "SJIS" "UTF-8"

toSJIS :: Text -> ByteString
toSJIS = convertFuzzy Discard "UTF-8" "SJIS" . encodeUtf8
