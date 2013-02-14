{-# LANGUAGE EmptyDataDecls       #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tonque.Model where

import           Data.Text.Lazy      (Text)
import           Database.Persist
import           Database.Persist.TH

import           Tonque.Type

share [mkPersist sqlSettings, mkMigrate "migrate"] [persist|
BoardGroupCache
    name   Text
    boards [BoardCache]
  deriving Show Read Eq Ord

BoardCache
    name   Text
    url    Text
  deriving Show Read Eq Ord

ThreadCache
    identifier  Text
    time        EpochTime
    title       Text
    resCount    Int  Update
    alreadyRead Int  Update
    isFav       Bool Update
    UniqueThreadCache identifier
  deriving Show Read Eq Ord

ResListCache
    resIds     ResIds
    resses     [Res]
  deriving Show Read Eq Ord
|]

derivePersistField "Text"
derivePersistField "EpochTime"
derivePersistField "ResIds"
derivePersistField "Res"

instance Cacheable Board BoardCache where
    toCache board = BoardCache (boardName board) (boardUrl board)
    fromCache cache = Board (boardCacheName cache) (boardCacheUrl cache)

instance Cacheable BoardGroup BoardGroupCache where
    toCache group = BoardGroupCache
        { boardGroupCacheName   = boardGroupName group
        , boardGroupCacheBoards = toCache $ boardGroupBoards group
        }
    fromCache group = BoardGroup
        { boardGroupName   = boardGroupCacheName group
        , boardGroupBoards = fromCache $ boardGroupCacheBoards group
        }

instance Cacheable Thread ThreadCache where
    toCache thread = ThreadCache
        { threadCacheIdentifier  = threadIdentifier thread
        , threadCacheTime        = threadTime thread
        , threadCacheTitle       = threadTitle thread
        , threadCacheResCount    = threadResCount thread
        , threadCacheAlreadyRead = threadAlreadyRead thread
        , threadCacheIsFav       = threadIsFav thread
        }
    fromCache thread = Thread
        { threadIdentifier  = threadCacheIdentifier thread
        , threadTime        = threadCacheTime thread
        , threadTitle       = threadCacheTitle thread
        , threadResCount    = threadCacheResCount thread
        , threadAlreadyRead = threadCacheAlreadyRead thread
        , threadIsFav       = threadCacheIsFav thread
        }

instance Cacheable ResList ResListCache where
    toCache resList = ResListCache
        { resListCacheResIds = resListResIds resList
        , resListCacheResses = resListResses resList
        }
    fromCache resList = ResList
        { resListResIds = resListCacheResIds resList
        , resListResses = resListCacheResses resList
        }
