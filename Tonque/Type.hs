module Tonque.Type where

import Data.Text (Text)
import Foreign.C.Types (CTime)

type EpochTime = CTime

type BoardName = Text
type URL = Text
type Board = (BoardName, URL)

type BoardGroupName = Text
type BoardGroup = (BoardGroupName, [Board])

type ThreadName = Text
type ThreadResCount = Int
type Thread = (EpochTime, ThreadName, ThreadResCount)
