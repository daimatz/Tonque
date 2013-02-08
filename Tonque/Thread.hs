module Tonque.Thread
    ( getThread
    )
    where

import Control.Monad (guard)
import Data.List (elemIndices)
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL

import Tonque.Type
import Tonque.Util

getThread :: Text -> Text -> Text -> IO ResList
getThread host path key = do
    thread <- request $  "http://" <> host <> "/" <> path <> "/dat/"
                      <> key <> ".dat"
    let resses = map parseLine $ zip [1..] $ TL.lines thread
        ids = resIds resses
    return (ids, resses)

-- | parse line and return Res.
--  when failed to parse, then returns Error value.
--
-- >>> parseLine (1, "a<><>2013/02/08(金) 16:02:11.12 ID:9oSML5U3<>hoge<>")
-- Res 1 "a" (read "2013-02-08 16:02:11.12") "9oSML5U3" "hoge"
parseLine :: (Int, Text) -> Res
parseLine (n, line)
    = case TL.splitOn delim line of
        [name, mail, date_id, title, body] ->
          let (date_idstr, id_) = TL.breakOnEnd idstr date_id
              date = maybe date_idstr id $ TL.stripSuffix idstr date_idstr
          in Res
            { resNumber = n
            , resName   = TL.replace "</b>" "" $ TL.replace "<b>" "" name
            , resMail   = mail
            , resDate   = date
            , resId     = id_
            , resTitle  = guard (body /= "") >> Just title
            , resBody   = if body /= "" then body else title
            }
        _ -> Res n "error" "" "error" "error" (Just "error") "error"
  where
    delim = "<>"
    idstr = " ID:"

-- | count id appearance times
--
-- >>> resIds [ Res 1 "" "" "" "a" ""
--            , Res 2 "" "" "" "a" ""
--            , Res 3 "" "" "" "b" ""
--            ]
-- Map.fromList [("a", 2), ("b", 1)]
resIds :: [Res] -> Map.Map Text Int
resIds ressess
    = let ids = [ resId v | v <- ressess ]
          indices = [ (n, elemIndices n ids) | n <- ids ]
      in Map.fromList [ (fst ns, length $ snd ns) | ns <- indices ]
