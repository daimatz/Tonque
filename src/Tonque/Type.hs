{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Tonque.Type where

import           Data.Map        (Map)
import           Data.Text.Lazy  (Text)
import           Foreign.C.Types (CTime)

type EpochTime = CTime

class Cacheable live cache where
    toCache :: live -> cache
    fromCache :: cache -> live

instance Cacheable live cache => Cacheable [live] [cache] where
    toCache = map toCache
    fromCache = map fromCache

data Board = Board
    { boardName :: Text
    , boardUrl  :: Text
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
