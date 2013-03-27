module Tonque.Thread
    ( getThread
    , getThreadByIdentifier
    )
    where

import           Control.Monad  (guard)
import           Data.List      (elemIndices)
import qualified Data.Map       as Map
import           Data.Monoid    ((<>))
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL

import           Tonque.Type
import           Tonque.Util    (request)

getThread :: Text -> Text -> Text -> IO ResList
getThread host path key = do
    thread <- request $  "http://" <> host <> "/" <> path <> "/dat/"
                      <> key <> ".dat"
    let resses = map parseLine $ zip [1..] $ TL.lines thread
        ids = resIds resses
    return $ ResList ids resses

getThreadByIdentifier :: Text -> IO ResList
getThreadByIdentifier identifier = do
    let [host, path, key] = TL.splitOn "/" identifier
    getThread host path key

-- | parse line and return Res.
--  when failed to parse, then returns Error value.
parseLine :: (Int, Text) -> Res
parseLine (n, line)
    = case TL.splitOn delim line of
        [name, mail, date_id, body, title] ->
          let (date_idstr, id_) = TL.breakOnEnd idstr date_id
              date = maybe date_idstr id $ TL.stripSuffix idstr date_idstr
          in Res
            { resNumber = n
            , resName   = TL.replace "</b>" "" $ TL.replace "<b>" "" name
            , resMail   = mail
            , resDate   = date
            , resId     = id_
            , resTitle  = guard (title /= "") >> Just title
            , resBody   = body
            }
        _ -> Res n "error" "" "error" "error" (Just "error") "error"
  where
    delim = "<>"
    idstr = " ID:"

-- | count id appearance times
resIds :: [Res] -> ResIds
resIds ressess
    = let ids = [ resId v | v <- ressess ]
          indices = [ (n, elemIndices n ids) | n <- ids ]
      in Map.fromList [ (fst ns, length $ snd ns) | ns <- indices ]
