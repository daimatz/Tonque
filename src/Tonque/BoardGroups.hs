module Tonque.BoardGroups
    ( updateBoardGroups
    , readBoardGroups
    )
    where

import           Control.Applicative       ((<$>), (<*>), (<|>))
import           Data.Attoparsec.Text.Lazy
import           Data.Text.Lazy            (Text)
import qualified Data.Text.Lazy            as TL

import           Tonque.DBUtil
import           Tonque.Type
import           Tonque.Util

boardGroupsUrl :: Text
boardGroupsUrl = "http://menu.2ch.net/bbstable.html"

-- | read board groups from DB
readBoardGroups :: IO [BoardGroup]
readBoardGroups = return . fromCache =<< readBoardGroupCaches

updateBoardGroups :: IO ()
updateBoardGroups = do
    html <- request boardGroupsUrl
    let html'  = TL.replace "【"  "/【" html -- FIXME
        groups = case parse allBoardParser html' of
                   Fail _ s t -> error $ show s ++ t
                   Done _ r   -> r
    updateBoardGroupCaches $ toCache groups

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
