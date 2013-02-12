{-# LANGUAGE CPP #-}

module Tonque.DBUtil where

import           Control.Applicative         ((<$>))
import           Control.Monad               (forM)
import           Data.Text.Lazy              (Text)
import           Database.Persist
import           Database.Persist.GenericSql

import           Tonque.Model

#define SQLite

#ifdef SQLite

import           Database.Persist.Sqlite     (withSqliteConn)

runSql :: SqlPersist IO a -> IO a
runSql = withSqliteConn dbfile . runSqlConn
  where
    dbfile = "db.sqlite3"

#else

import           Data.Monoid                 ((<>))
import           Data.Text.Encoding          (encodeUtf8)
import           Database.Persist.Postgresql (withPostgresqlConn)
import           Database.Persist.Store      (applyEnv, loadConfig)
import           Web.Heroku                  (dbConnParams)

runSql :: SqlPersist IO a -> IO a
runSql query = do
    params <- dbConnParams
    let connStr = foldr (\(k,v) t -> t <> (encodeUtf8 $ k <> "=" <> v <> " "))
                  "" params
    withPostgresqlConn connStr $ runSqlConn query

#endif

readBoardGroupCaches :: IO [BoardGroupCache]
readBoardGroupCaches
    = map entityVal <$> (runSql $ selectList [] [Asc BoardGroupCacheId])

updateBoardGroupCaches :: [BoardGroupCache] -> IO ()
updateBoardGroupCaches boards = do
    runSql $ deleteWhere ([] :: [Filter BoardGroupCache])
    _ <- runSql $ forM boards $ \board -> insert board
    return ()

readThreadCaches :: IO [ThreadCache]
readThreadCaches
    = map entityVal <$> (runSql $ selectList [] [Asc ThreadCacheId])

readThreadCache :: Text -> IO (Maybe ThreadCache)
readThreadCache identifier = do
    entity <- runSql $ selectFirst [ThreadCacheIdentifier ==. identifier] []
    return $ entityVal <$> entity

addThreadCache :: ThreadCache -> IO ()
addThreadCache thread = do
    runSql $ insert thread
    return ()

updateThreadCache :: ThreadCache -> IO ()
updateThreadCache thread = runSql
    $ updateWhere [ThreadCacheIdentifier ==. threadCacheIdentifier thread]
        [ ThreadCacheResCount    =. threadCacheResCount thread
        , ThreadCacheAlreadyRead =. threadCacheAlreadyRead thread
        , ThreadCacheIsFav       =. threadCacheIsFav thread
        ]

removeThreadCache :: Text -> IO ()
removeThreadCache identifier = do
    runSql $ deleteWhere [ThreadCacheIdentifier ==. identifier]
    return ()

readFavThreads :: IO [ThreadCache]
readFavThreads
    = map entityVal <$> (runSql $ selectList [ThreadCacheIsFav ==. True] [Asc ThreadCacheId])
