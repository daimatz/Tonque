module Tonque.Type where

import Data.Text (Text)

type BoardName = Text
type URL = Text
type Board = (BoardName, URL)

type BoardGroupName = Text
type BoardGroup = (BoardGroupName, [Board])

type ThreadName = Text
type ThreadKey = Integer -- UNIX Time (sec) of thread-built date
type ThreadResCount = Int
type Thread = (ThreadKey, ThreadName, ThreadResCount)
