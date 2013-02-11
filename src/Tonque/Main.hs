import           Control.Applicative         ((<$>))
import           Control.Monad.IO.Class      (liftIO)
import           Database.Persist.GenericSql (runMigration)
import           System.Environment          (getEnv)
import           Web.Scotty                  hiding (body)

import           Tonque.Board
import           Tonque.BoardGroups
import           Tonque.DBUtil
import           Tonque.HTML
import           Tonque.Model
import           Tonque.Thread

main :: IO ()
main = do
    runSql $ runMigration migrate
    port <- read <$> getEnv "PORT"
    scotty port $ do
        get "/" $ do
            groups <- liftIO readBoardGroups
            body =<< boardGroupsHTML groups
        get "/updateBoardGroups" $ do
            liftIO updateBoardGroups
            redirect "/"
        get "/board/:host/:path" $ \ host path -> do
            threads <- liftIO $ getBoard host path
            body =<< boardHTML host path threads
        get "/thread/:host/:path/:key" $ \ host path key -> do
            resList <- liftIO $ getThread host path key
            body =<< threadHTML resList
