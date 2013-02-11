import           Control.Applicative         ((<$>))
import           Control.Monad.IO.Class      (liftIO)
import           Data.Monoid                 ((<>))
import qualified Data.Text.Lazy              as TL
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

        get "/board/:host/:path/index" $ \ host path -> do
            threads <- liftIO $ getBoard host path
            body =<< boardHTML threads
        get "/board/:host/:path/:key/index" $ \ host path key -> do
            resList <- liftIO $ getThread host path key
            let url = threadId host path key
            faved <- liftIO $ searchFavThread url
            body =<< threadHTML faved resList
        get "/board/:host/:path/:key/fav/:alreadyRead"
            $ \ host path key alreadyRead -> do
            let url = threadId host path key
                thread = FavThread url (read $ TL.unpack alreadyRead)
            liftIO $ addFavThread thread
            redirect $ "/board/" <> url <> "/index"
        get "/board/:host/:path/:key/unfav" $ \ host path key -> do
            let url = threadId host path key
            liftIO $ removeFavThread url
            redirect $ "/board/" <> url <> "/index"
  where
    threadId host path key = host <> "/" <> path <> "/" <> key
