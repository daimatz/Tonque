module Tonque.HTML
    ( threadListHTML
    , boardListHTML
    )
    where

import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T

import Tonque.Board
import Tonque.BoardList
import Tonque.Util

boardListHTML :: IO Text
boardListHTML = do
    boards <- readBoardList
    return $  "<ul class=\"boardlist\">"
           <> (T.concat $ map groupHTML boards)
           <> "</ul>"
  where
    groupHTML group =  "<li>"
                    <> fst group
                    <> "<ul>\n"
                    <> (T.concat $ map boardHTML $ snd group)
                    <> "</ul></li>\n"
    boardHTML board =  "<li><a href=\""
                    <> uri
                    <> "\">"
                    <> fst board
                    <> "</a></li>\n"
      where
        uri = "/board/" <> arg
        arg = T.drop 7 $ snd board -- drop "http://"


threadListHTML :: Text -> Text -> IO Text
threadListHTML host path = do
    threads <- getThreadList host path
    return $  "<ul class=\"threadlist\">\n"
           <> (T.concat $ map f threads)
           <> "</ul>"
  where
    f (time, name, cnt)
      =  "<li><a href=\""
      <> uri
      <> "\">"
      <> name
      <> "</a>\n<ul>\n<li>"
      <> textShow cnt
      <> "</li>\n<li>"
      <> (timeFormat $ epochToUTC time)
      <> "</li>\n</ul>\n</li>\n"
      where
        uri = "/thread/" <> host <> "/" <> path <> "/" <> textShow time


