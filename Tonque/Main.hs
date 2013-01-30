{-# LANGUAGE OverloadedStrings #-}

import Tonque.BBS

import System.Environment (getEnv)
import Data.Monoid (mconcat)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Control.Applicative ((<$>))
import Web.Scotty

main = do
  port <- read <$> getEnv "PORT"
  str <- getListHTML
  scotty port $ do
    get "/" $ do
      html $ TL.pack $ T.unpack str
    get "/bbs" $ do
      html "OK"
