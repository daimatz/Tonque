import Control.Monad.IO.Class
import System.Environment (getEnv)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Control.Applicative ((<$>))
import Web.Scotty

import Tonque.HTML

main :: IO ()
main = do
  port <- read <$> getEnv "PORT"
  scotty port $ do
    get "/" $ do
      boards <- liftIO boardListHTML
      html $ TL.fromStrict boards
    get "/board/:host/:path" $ do
      host <- T.pack <$> param "host"
      path <- T.pack <$> param "path"
      threads <- liftIO $ threadListHTML host path
      html $ TL.fromStrict threads
