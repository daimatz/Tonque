module Tonque.Type where

import Data.Text (Text)

type BoardName = Text
type URL = Text
type Board = (BoardName, URL)

type BoardGroupName = Text
type BoardGroup = (BoardGroupName, [Board])