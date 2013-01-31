{-# LANGUAGE OverloadedStrings #-}

import Tonque.BBS

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
      bbss <- liftIO allBBSListHTML
      html $ TL.pack $ T.unpack bbss
    get "/bbs" $ do
      html "OK"
