module Tonque.BoardList
    ( updateBoardList
    , allBoardList
    , allBoardListHTML
    )
    where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T

import Tonque.Request
import Tonque.Type

boardListURL :: URL
boardListURL = "http://menu.2ch.net/bbstable.html"

updateBoardList :: IO [BoardGroup]
updateBoardList = getBoardList

allBoardList :: IO [BoardGroup]
allBoardList = getBoardList

allBoardListHTML :: IO Text
allBoardListHTML = do
    allBoard <- allBoardList
    return $ "<ul>" <> (T.concat $ map groupHTML allBoard) <> "</ul>"
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

getBoardList :: IO [BoardGroup]
getBoardList = do
    html <- getListHTML
    let html' = T.replace "【"  "/【" html -- FIXME
    case parse allBoardParser html' of
      Fail _ s t -> error $ show s ++ t
      Partial _  -> error "Unknown"
      Done _ r   -> return r

getListHTML :: IO Text
getListHTML = request boardListURL

allBoardParser :: Parser [BoardGroup]
allBoardParser = do
    manyTill anyChar (try $ string "【")
    many1 groupParser >>= return

groupParser :: Parser BoardGroup
groupParser = do
    string "<B>"
    name <- (:) <$> anyChar <*> manyTill anyChar (try $ string "</B>】")
    boards <- manyTill boardParser $ try (string "【" <|> string "更新日")
    return (T.pack name, boards)

boardParser :: Parser Board
boardParser = do
    manyTill anyChar (try $ string "<A HREF=")
    url  <-  (:)
         <$> anyChar
         <*> manyTill anyChar (try $ string ">")
    name <-  (:)
         <$> anyChar
         <*> manyTill anyChar (try $ string "</A>")
    manyTill anyChar $ char '/'
    return (T.pack name, T.pack $ fst $ break (== ' ') url)
