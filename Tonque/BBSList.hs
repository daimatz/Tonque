module Tonque.BBSList
    ( updateBBSList
    , allBBSList
    , allBBSListHTML
    )
    where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T

import Tonque.Request
import Tonque.Type

bbsListURL :: URL
bbsListURL = "http://menu.2ch.net/bbstable.html"

updateBBSList :: IO [BBSGroup]
updateBBSList = getAllBBSList

allBBSList :: IO [BBSGroup]
allBBSList = getAllBBSList

allBBSListHTML :: IO Text
allBBSListHTML = do
    all_bbs <- allBBSList
    return $ "<ul>" <> (T.concat $ map groupHTML all_bbs) <> "</ul>"
  where
    groupHTML group =  "<li>"
                    <> fst group
                    <> "<ul>\n"
                    <> (T.concat $ map bbsHTML $ snd group)
                    <> "</ul></li>\n"
    bbsHTML   bbs   =  "<li><a href=\""
                    <> uri
                    <> "\">"
                    <> fst bbs
                    <> "</a></li>\n"
      where
        uri = "/bbs/" <> arg
        arg = T.drop 7 $ snd bbs -- drop "http://"

getAllBBSList :: IO [BBSGroup]
getAllBBSList = do
    html <- getListHTML
    let html' = T.replace "【"  "/【" html -- FIXME
    case parse allBBSParser html' of
      Fail _ s t -> error $ show s ++ t
      Partial _  -> error "Unknown"
      Done _ r   -> return r

getListHTML :: IO Text
getListHTML = request bbsListURL

allBBSParser :: Parser [BBSGroup]
allBBSParser = do
    manyTill anyChar (try $ string "【")
    many1 groupParser >>= return

groupParser :: Parser BBSGroup
groupParser = do
    string "<B>"
    name <- (:) <$> anyChar <*> manyTill anyChar (try $ string "</B>】")
    bbss <- manyTill bbsParser $ try (string "【" <|> string "更新日")
    return (T.pack name, bbss)

bbsParser :: Parser BBS
bbsParser = do
    manyTill anyChar (try $ string "<A HREF=")
    url  <-  (:)
         <$> anyChar
         <*> manyTill anyChar (try $ string ">")
    name <-  (:)
         <$> anyChar
         <*> manyTill anyChar (try $ string "</A>")
    manyTill anyChar $ char '/'
    return (T.pack name, T.pack $ fst $ break (== ' ') url)
