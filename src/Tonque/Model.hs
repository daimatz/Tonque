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

FavThread
    identifier  Text
    alreadyRead Int
  deriving Show Read Eq Ord
|]

derivePersistField "Text"
derivePersistField "EpochTime"

instance Cacheable Board BoardCache where
    toCache board = BoardCache (boardName board) (boardUrl board)
    fromCache cache = Board (boardCacheName cache) (boardCacheUrl cache)

instance Cacheable BoardGroup BoardGroupCache where
    toCache group = BoardGroupCache
                        (boardGroupName group)
                        (map toCache $ boardGroupBoards group)
    fromCache group = BoardGroup
                        (boardGroupCacheName group)
                        (map fromCache $ boardGroupCacheBoards group)
