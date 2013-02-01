module Tonque.Util where

import Data.Convertible (safeConvert)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)

import Tonque.Type

textShow :: Show a => a -> Text
textShow = T.pack . show

epochToUTC :: EpochTime -> UTCTime
epochToUTC t = case safeConvert t of
    Left err  -> error $ show err
    Right res -> res

timeFormat :: UTCTime -> Text
timeFormat = T.pack . formatTime defaultTimeLocale "%F %T"
