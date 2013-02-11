module Tonque.Type where

import Data.Text.Lazy (Text)
import Data.Map (Map)
import Foreign.C.Types (CTime)

type EpochTime = CTime
type URL = Text

data Board = Board
    { boardName :: Text
    , boardURL  :: Text
    }
  deriving (Show, Read, Eq, Ord)

data BoardGroup = BoardGroup
    { boardGroupName   :: Text
    , boardGroupBoards :: [Board]
    }
  deriving (Show, Read, Eq, Ord)

data Thread = Thread
    { threadTime     :: EpochTime
    , threadName     :: Text
    , threadResCount :: Int
    }
  deriving (Show, Read, Eq, Ord)

data Res = Res
    { resNumber :: Int
    , resName   :: Text
    , resMail   :: Text
    , resDate   :: Text
    , resId     :: Text
    , resTitle  :: Maybe Text
    , resBody   :: Text
    }
  deriving (Show, Read, Eq, Ord)

data ResList = ResList
    { resIds        :: Map Text Int
    , resListResses :: [Res]
    }
  deriving (Show, Read, Eq, Ord)
