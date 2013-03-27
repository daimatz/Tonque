module Tonque.Board
    ( getBoard
    , threadId
    )
    where

import           Control.Applicative       (many)
import           Data.Attoparsec.Text.Lazy
import           Data.Char                 (isDigit)
import           Data.Monoid               ((<>))
import           Data.Text.Lazy            (Text)
import qualified Data.Text.Lazy            as TL

import           Tonque.Type
import           Tonque.Util               (request)

boardPath :: Text
boardPath = "/subject.txt"

getBoard :: Text -> Text -> IO [Thread]
getBoard host path = do
    board <- request $ "http://" <> host <> "/" <> path <> boardPath
    case parse (boardParser host path) board of
      Fail _ s t -> error $ show s ++ t
      Done _ r   -> return r

boardParser :: Text -> Text -> Parser [Thread]
boardParser host path = do
    many $ threadParser host path

threadParser :: Text -> Text -> Parser Thread
threadParser host path = do
    time <- many digit
    string ".dat<>"
    rest <- takeTill (flip elem "\r\n")
    let (name, numStr) = TL.breakOnEnd " " $ TL.fromStrict rest
        num = TL.init $ TL.tail numStr
    many (satisfy $ not . isDigit)
    return $ Thread
        { threadIdentifier  = threadId host path (TL.pack time)
        , threadTime        = read time
        , threadTitle       = TL.strip name
        , threadResCount    = read $ TL.unpack num
        , threadAlreadyRead = 0
        , threadIsFav       = False
        }

threadId :: Text -> Text -> Text -> Text
threadId host path key = host <> "/" <> path <> "/" <> key
