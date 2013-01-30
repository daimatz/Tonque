module Tonque.Type where

import Data.Text (Text)

type BBSName = Text
type URL = Text
type BBS = (BBSName, URL)

type BBSGroupName = Text
type BBSGroup = (BBSGroupName, [BBS])
