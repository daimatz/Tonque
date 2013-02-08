module Tonque.HTML
    ( boardHTML
    , boardListHTML
    , threadHTML
    )
    where

import Data.Map ((!))
import Data.Monoid ((<>))
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL

import Tonque.Board
import Tonque.BoardList
import Tonque.Thread
import Tonque.Type
import Tonque.Util

threadHTML :: Text -> Text -> Text -> IO Text
threadHTML host path key = do
    (ids, resses) <- getThread host path key
    return $  "<ul class=\"thread\">\n"
           <> (TL.concat $ map (f ids) resses)
           <> "</ul>"
  where
    f ids res
      = "<li class=\"resNumber\">"
      <> textShow (resNumber res)
      <> "<ul class=\"resContent\">\n<li class=\"resName\">"
      <> resName res
      <> "</li>\n<li class=\"resDate\">"
      <> resDate res
      <> "</li>\n<li class=\"resId\">"
      <> resId res
      <> "</li><li class=\"resIdCount\">("
      <> textShow (ids ! resId res)
      <> ")</li>\n<li class=\"resBody\">"
      <> resBody res
      <> "</li>\n</ul></li>\n"

boardHTML :: Text -> Text -> IO Text
boardHTML host path = do
    board <- getBoard host path
    return $  "<ul class=\"board\">\n"
           <> (TL.concat $ map f board)
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
           <> (TL.concat $ map groupHTML boards)
           <> "</ul>"
  where
    groupHTML (name, boards)
        =  "<li>"
        <> name
        <> "<ul>\n"
        <> (TL.concat $ map boardLinkHTML boards)
        <> "</ul></li>\n"
    boardLinkHTML (name, url)
        =  "<li><a href=\""
        <> "/board/" <> TL.drop 7 url -- drop "http://"
        <> "\">"
        <> name
        <> "</a></li>\n"
