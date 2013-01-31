module Tonque.Board
    ( getThreadList
    , getThreadListHTML
    )
    where

import Data.Text (Text)

import Tonque.Request
import Tonque.Type

threadListPath :: Text
threadListPath = "/subject.txt"

getThreadList :: Text -> Text -> IO [Thread]
getThreadList host path = return []

getThreadListHTML :: Text -> Text -> IO Text
getThreadListHTML host path = return ""
