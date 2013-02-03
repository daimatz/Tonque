module Tonque.Board
    ( getBoard
    )
    where

import Control.Applicative
import Data.Char
import Data.Attoparsec.Text
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T

import Tonque.Type
import Tonque.Util

boardPath :: Text
boardPath = "/subject.txt"

getBoard :: Text -> Text -> IO [Thread]
getBoard host path = do
    board <- request $ "http://" <> host <> "/" <> path <> boardPath
    case parse boardParser board of
      Fail _ s t -> error $ show s ++ t
      Partial f  -> let Done _ r' = f "" in return r'
      Done _ r   -> return r

boardParser :: Parser [Thread]
boardParser = do
    many threadParser

threadParser :: Parser Thread
threadParser = do
    time <- many digit
    string ".dat<>"
    rest <- takeTill (flip elem "\r\n")
    let (name, numStr) = T.breakOnEnd " " rest
        num = T.init $ T.tail numStr
    many (satisfy $ not . isDigit)
    return (read time, T.strip name, read $ T.unpack num)
