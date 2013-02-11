module Tonque.Board
    ( getBoard
    )
    where

import           Control.Applicative       (many)
import           Data.Attoparsec.Text.Lazy
import           Data.Char                 (isDigit)
import           Data.Monoid               ((<>))
import           Data.Text.Lazy            (Text)
import qualified Data.Text.Lazy            as TL

import           Tonque.Type
import           Tonque.Util

boardPath :: Text
boardPath = "/subject.txt"

getBoard :: Text -> Text -> IO [Thread]
getBoard host path = do
    board <- request $ "http://" <> host <> "/" <> path <> boardPath
    case parse boardParser board of
      Fail _ s t -> error $ show s ++ t
      Done _ r   -> return r

boardParser :: Parser [Thread]
boardParser = do
    many threadParser

threadParser :: Parser Thread
threadParser = do
    time <- many digit
    string ".dat<>"
    rest <- takeTill (flip elem "\r\n")
    let (name, numStr) = TL.breakOnEnd " " $ TL.fromStrict rest
        num = TL.init $ TL.tail numStr
    many (satisfy $ not . isDigit)
    return $ Thread (read time) (TL.strip name) (read $ TL.unpack num)
