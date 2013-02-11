{-# LANGUAGE EmptyDataDecls       #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tonque.Model where

import           Data.Text.Lazy      (Text)
import           Database.Persist
import           Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrate"] [persist|
BoardGroupCache
    name   Text
    boards [BoardCache]
  deriving Show Read Eq Ord

BoardCache
    name   Text
    url    Text
  deriving Show Read Eq Ord
|]

derivePersistField "Text"
