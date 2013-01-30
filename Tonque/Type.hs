module Tonque.Type where

import Data.Text (Text)

type Name = Text
type URL = Text
type BBS = (Name, URL)
type BBSGroup = [BBS]
