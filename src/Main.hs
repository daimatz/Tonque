module Main where

import           Control.Applicative         ((<$>))
import           Database.Persist.GenericSql (runMigration)
import           System.Environment          (getEnv)
import           Web.Scotty                  (scotty)

import           Tonque.App
import           Tonque.DBUtil
import           Tonque.Model

main :: IO ()
main = do
    runSql $ runMigration migrate
    port <- read <$> getEnv "PORT"
    scotty port app
