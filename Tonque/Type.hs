module Tonque.Type where

import Data.Text.Lazy (Text)
import Data.Map (Map)
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

data Res = Res
  { resNumber :: Int
  , resName   :: Text
  , resMail   :: Text
  , resDate   :: Text
  , resId     :: Text
  , resTitle  :: Maybe Text
  , resBody   :: Text
  }
type ResIds = Map Text Int
type ResList = (ResIds, [Res])
