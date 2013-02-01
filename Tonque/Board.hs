module Tonque.Board
    ( getThreadList
    , getThreadListHTML
    )
    where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Attoparsec.Text as AT
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T

import Tonque.Request
import Tonque.Type

threadListPath :: Text
threadListPath = "/subject.txt"

getThreadList :: Text -> Text -> IO [Thread]
getThreadList host path = do
    threads <- request $ "http://" <> host <> "/" <> path <> threadListPath
    case parse (many threadParser) threads of
      Fail _ s t -> error $ show s ++ t
      Partial _  -> error "Unknown"
      Done _ r   -> return r

getThreadListHTML :: Text -> Text -> IO Text
getThreadListHTML host path = do
    threads <- getThreadList host path
    return $ "<ul>" <> (T.concat $ map f threads) <> "</ul>"
  where
    f (key, name, cnt)
      = "<li><a href=\"" <> uri <> ">" <> name <> "</a></li>\n"
      where
        uri = "/thread/" <> (T.pack $ show key)

threadParser :: Parser Thread
threadParser = do
    skipSpace
    key <- keyParser
    string ".dat<>"
    name <- AT.takeWhile (not . isEndOfLine)
    return (key, name, 1)

keyParser :: Parser Integer
keyParser = inFix digitByte

inFix :: Parser a -> Parser a
inFix p = try p <|> do anyChar; inFix p

digitByte :: Parser Integer
digitByte = do
    ns <- many1 $ do c <- digit ; return $ digitToInt c
    when (head ns == 0) $ error "Illegal Key"
    return $ nsToInteger ns

nsToInteger :: [Int] -> Integer
nsToInteger = fromIntegral . foldl (\x y -> x * 10 + y) 0
