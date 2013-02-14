{-# LANGUAGE RankNTypes #-}

module Tonque.HTML
    ( boardGroupsHTML
    , boardHTML
    , body
    , threadHTML
    )
    where

import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Data.Map                ((!))
import           Data.Monoid             ((<>))
import qualified Data.Text               as T
import           Data.Text.Lazy          (Text)
import qualified Data.Text.Lazy          as TL
import           Data.Text.Lazy.Encoding (decodeUtf8)
import           Text.Hastache
import           Text.Hastache.Context
import           Web.Scotty              (ActionM, html)

import           Tonque.Type
import           Tonque.Util

mustache :: MonadIO m => FilePath -> MuContext m -> m Text
mustache path context = return . decodeUtf8
    =<< hastacheFile config path context
  where
    config = defaultConfig
           { muEscapeFunc = emptyEscape
           }

nullContext :: MonadIO m => MuContext m
nullContext = mkStrContext $ const $ MuVariable ("" :: T.Text)

errorRef :: forall m . MuType m
errorRef = MuVariable ("error" :: T.Text)

threadHTML :: Bool -> ResList -> ActionM Text
threadHTML faved (ResList ids resses)
    = liftIO $ mustache "view/Thread.mustache" (mkStrContext context)
  where
    context "res"     = MuList $ map (mkStrContext . resContext) resses
    context "favUrl"  = MuVariable $ if faved then "unfav" else ("fav" :: Text)
    context "favText" = MuVariable $ if faved then "unfav" else ("fav" :: Text)
    context _         = errorRef
    resContext res "resNumber"  = MuVariable $ textShow (resNumber res)
    resContext res "resName"    = MuVariable $ resName res
    resContext res "resDate"    = MuVariable $ resDate res
    resContext res "resId"      = MuVariable $ resId res
    resContext res "resIdCount" = MuVariable $ textShow (ids ! resId res)
    resContext res "resBody"    = MuVariable $ resBody res
    resContext _   _            = errorRef

boardHTML :: [Thread] -> ActionM Text
boardHTML threads
    = liftIO $ mustache "view/Board.mustache" (mkStrContext context)
  where
    context "threads" = MuList $ map (mkStrContext . threadContext) threads
    context _         = errorRef
    threadContext thread "threadUrl"
        = MuVariable $ "/board/" <> threadIdentifier thread <> "/index"
    threadContext thread "threadTitle"
        = MuVariable $ threadTitle thread
    threadContext thread "threadResCount"
        = MuVariable $  textShow (threadAlreadyRead thread)
                     <> " / "
                     <>  textShow (threadResCount thread)
    threadContext thread "threadCreated"
        = MuVariable $ timeFormat $ epochToUTC $ threadTime thread
    threadContext _ _
        = errorRef

boardGroupsHTML :: [BoardGroup] -> ActionM Text
boardGroupsHTML groups
    = liftIO $ mustache "view/BoardGroups.mustache" (mkStrContext context)
  where
    context "boardGroup" = MuList $ map (mkStrContext . groupContext) groups
    context _            = errorRef
    groupContext (BoardGroup groupName _) "groupName"
        = MuVariable groupName
    groupContext (BoardGroup _ boards) "boards"
        = MuList $ map (mkStrContext . boardContext) boards
    groupContext _     _
        = errorRef
    boardContext (Board name _) "boardName"
        = MuVariable name
    boardContext (Board _ url) "boardURL"
        = MuVariable $ "/board/" <> TL.drop (TL.length "http://") url <> "index"
    boardContext _     _
        = errorRef

body :: Text -> ActionM ()
body text = do
    h <- liftIO $ mustache "view/Head.mustache" nullContext
    f <- liftIO $ mustache "view/Foot.mustache" nullContext
    html $ h <> text <> f
