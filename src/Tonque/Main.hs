import Control.Monad.IO.Class (liftIO)
import System.Environment (getEnv)
import Control.Applicative ((<$>))
import Web.Scotty hiding (body)

import Tonque.Board
import Tonque.BoardList
import Tonque.HTML
import Tonque.Thread

main :: IO ()
main = do
  port <- read <$> getEnv "PORT"
  scotty port $ do
    get "/" $ do
      groups <- liftIO readBoardList
      body =<< boardListHTML groups
    get "/board/:host/:path" $ \ host path -> do
      threads <- liftIO $ getBoard host path
      body =<< boardHTML host path threads
    get "/thread/:host/:path/:key" $ \ host path key -> do
      resList <- liftIO $ getThread host path key
      body =<< threadHTML resList
