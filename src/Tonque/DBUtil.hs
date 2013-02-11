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

searchFavThread :: Text -> IO Bool
searchFavThread identifier = do
    lst <- map entityVal <$>
        (runSql $ selectList [FavThreadIdentifier ==. identifier] [])
    return $ lst /= []

addFavThread :: FavThread -> IO ()
addFavThread thread = do
    runSql $ insert thread
    return ()

removeFavThread :: Text -> IO ()
removeFavThread identifier = do
    runSql $ deleteWhere [FavThreadIdentifier ==. identifier]
    return ()
