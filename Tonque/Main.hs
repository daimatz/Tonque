{-# LANGUAGE OverloadedStrings #-}

import Tonque.BoardList

import Control.Monad.IO.Class
import System.Environment (getEnv)
import Data.Monoid (mconcat)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Control.Applicative ((<$>))
import Web.Scotty

main :: IO ()
main = do
  port <- read <$> getEnv "PORT"
  scotty port $ do
    get "/" $ do
      boards <- liftIO allBoardListHTML
      html $ TL.pack $ T.unpack boards
    get "/board" $ do
      html "OK"
