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
      boards <- liftIO boardListHTML
      html boards
    get "/board/:host/:path" $ do
      host <- param "host"
      path <- param "path"
      threads <- liftIO $ boardHTML host path
      html threads
