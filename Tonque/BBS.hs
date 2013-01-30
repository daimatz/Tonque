module Tonque.BBS where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec

import Tonque.Request
import Tonque.Type

bbsListURL :: URL
bbsListURL = "http://menu.2ch.net/bbstable.html"

getListHTML :: IO Text
getListHTML = request bbsListURL
