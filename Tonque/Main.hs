import Control.Monad.IO.Class (liftIO)
import System.Environment (getEnv)
import Control.Applicative ((<$>))
import Web.Scotty

import Tonque.HTML

main :: IO ()
main = do
  port <- read <$> getEnv "PORT"
  scotty port $ do
    get "/" $ do
      boardList <- liftIO boardListHTML
      html boardList
    get "/board/:host/:path" $ \ host path -> do
      board <- liftIO $ boardHTML host path
      html board
    get "/thread/:host/:path/:key" $ \ host path key -> do
      thread <- liftIO $ threadHTML host path key
      html thread
