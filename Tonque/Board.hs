module Tonque.Board
    ( getThreadList
    , getThreadListHTML
    )
    where

import Control.Applicative
import Data.Char
import Data.Attoparsec.Text
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T

import Tonque.Request
import Tonque.Type
import Tonque.Util

threadListPath :: Text
threadListPath = "/subject.txt"

getThreadList :: Text -> Text -> IO [Thread]
getThreadList host path = do
    threads <- request $ "http://" <> host <> "/" <> path <> threadListPath
    case parse allThreadParser threads of
      Fail _ s t -> error $ show s ++ t
      Partial f  -> let Done _ r' = f "" in return r'
      Done _ r   -> return r

getThreadListHTML :: Text -> Text -> IO Text
getThreadListHTML host path = do
    threads <- getThreadList host path
    return $  "<div class=\"threadlist\">\n"
           <> (T.concat $ map f threads)
           <> "</div>"
  where
    f (time, name, cnt)
      =  "<div><ul>\n<li><a href=\""
      <> uri
      <> "\">"
      <> name
      <> "</a></li>\n<li>"
      <> textShow cnt
      <> "</li>\n<li>"
      <> (timeFormat $ epochToUTC time)
      <> "</li>\n</ul></div>\n"
      where
        uri = "/thread/" <> host <> "/" <> path <> "/" <> textShow time

allThreadParser :: Parser [Thread]
allThreadParser = do
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
