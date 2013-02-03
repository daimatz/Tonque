module Tonque.HTML
    ( boardHTML
    , boardListHTML
    )
    where

import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T

import Tonque.Board
import Tonque.BoardList
import Tonque.Util

boardHTML :: Text -> Text -> IO Text
boardHTML host path = do
    board <- getBoard host path
    return $  "<ul class=\"board\">\n"
           <> (T.concat $ map f board)
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

boardListHTML :: IO Text
boardListHTML = do
    boards <- readBoardList
    return $  "<ul class=\"boardlist\">"
           <> (T.concat $ map groupHTML boards)
           <> "</ul>"
  where
    groupHTML (name, boards)
        =  "<li>"
        <> name
        <> "<ul>\n"
        <> (T.concat $ map boardLinkHTML boards)
        <> "</ul></li>\n"
    boardLinkHTML (name, url)
        =  "<li><a href=\""
        <> "/board/" <> T.drop 7 url -- drop "http://"
        <> "\">"
        <> name
        <> "</a></li>\n"
