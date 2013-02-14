module Tonque.App
    ( app
    )
    where

import           Control.Applicative    ((<$>))
import           Control.Monad          (forM, forM_)
import           Control.Monad.IO.Class (liftIO)
import           Data.Monoid            ((<>))
import qualified Data.Text.Lazy         as TL
import           Web.Scotty             hiding (body)

import           Tonque.Board
import           Tonque.BoardGroups
import           Tonque.DBUtil
import           Tonque.HTML
import           Tonque.Model
import           Tonque.Thread
import           Tonque.Type

app :: ScottyM ()
app = do

    get "/" $ do
        groups <- liftIO readBoardGroups
        body =<< boardGroupsHTML groups

    get "/favThreads" $ do
        threads <- fromCache <$> liftIO readFavThreads
        resses <- forM threads $ \thread -> liftIO
            $ getThreadByIdentifier (threadIdentifier thread)
        let pairs = zip threads (map (length . resListResses) resses)
        forM_ pairs $ \ (th, num) -> liftIO
            $ updateThreadCache $ toCache $ th { threadResCount = num }
        body =<< boardHTML threads

    get "/updateBoardGroups" $ do
        liftIO updateBoardGroups
        redirect "/"

    get "/board/:host/:path/index" $ \ host path -> do
        threads <- liftIO $ getBoard host path
        body =<< boardHTML threads
    get "/board/:host/:path/:key/index" $ \ host path key -> do
        resList@(ResList _ resses) <- liftIO $ getThread host path key
        let identifier = threadId host path key
        cache <- liftIO $ readThreadCache identifier
        isFav <- liftIO $ case cache of
          Nothing -> do
              addThreadCache $ ThreadCache
                { threadCacheIdentifier  = identifier
                , threadCacheTime        = read $ TL.unpack key
                , threadCacheTitle       = maybe "error" id $ resTitle (resses !! 0)
                , threadCacheResCount    = length resses
                , threadCacheAlreadyRead = length resses
                , threadCacheIsFav       = False
                }
              return False
          Just thread -> do
              updateThreadCache $ thread
                { threadCacheResCount    = length resses
                , threadCacheAlreadyRead = length resses
                }
              return $ threadCacheIsFav thread
        body =<< threadHTML isFav resList
    get "/board/:host/:path/:key/fav" $ \ host path key -> do
        let identifier = threadId host path key
        cache <- liftIO $ readThreadCache identifier
        liftIO $ case cache of
          Nothing -> error "No ThreadCache found"
          Just thread -> updateThreadCache $ thread
              { threadCacheIsFav = True
              }
        redirect $ "/board/" <> identifier <> "/index"
    get "/board/:host/:path/:key/unfav" $ \ host path key -> do
        let identifier = threadId host path key
        cache <- liftIO $ readThreadCache identifier
        liftIO $ case cache of
          Nothing -> error "No ThreadCache found"
          Just thread -> updateThreadCache $ thread
              { threadCacheIsFav = False
              }
        redirect $ "/board/" <> identifier <> "/index"
