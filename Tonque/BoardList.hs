module Tonque.BoardList
    ( updateBoardList
    , readBoardList
    )
    where

import Control.Applicative ((<$>), (<*>), (<|>))
import Data.Attoparsec.Text.Lazy
import qualified Data.Text.Lazy as TL

import Tonque.Type
import Tonque.Util

boardListURL :: URL
boardListURL = "http://menu.2ch.net/bbstable.html"

-- | read board list from DB
readBoardList :: IO [BoardGroup]
readBoardList = updateBoardList

updateBoardList :: IO [BoardGroup]
updateBoardList = do
    html <- request boardListURL
    let html' = TL.replace "【"  "/【" html -- FIXME
    case parse allBoardParser html' of
      Fail _ s t -> error $ show s ++ t
      Done _ r   -> return r

allBoardParser :: Parser [BoardGroup]
allBoardParser = do
    manyTill anyChar (try $ string "【")
    many1 groupParser

groupParser :: Parser BoardGroup
groupParser = do
    string "<B>"
    name <- (:) <$> anyChar <*> manyTill anyChar (try $ string "</B>】")
    boards <- manyTill boardParser $ try (string "【" <|> string "更新日")
    return $ BoardGroup (TL.pack name) boards

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
    return $ Board (TL.pack name) (TL.pack $ fst $ break (== ' ') url)
