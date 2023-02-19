{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

import Conduit ((.|))
import Conduit qualified as C
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Data.Aeson
import Data.Binary.Builder qualified as B
import Data.ByteString.Lazy.Char8 qualified as L
import Data.Text qualified as T
import Network.Wai.EventSource.EventStream
import Yesod.Core
import Yesod.EventSource

newtype Natural = Natural Int deriving (Eq, Show, Read)

instance PathPiece Natural where
  toPathPiece (Natural i) = T.pack $ show i
  fromPathPiece s =
    case reads $ T.unpack s of
      (i, "") : _
        | i < 1 -> Nothing
        | otherwise -> Just $ Natural i
      _ -> Nothing

newtype Counter = Counter Int

instance ToJSON Counter where
  toJSON (Counter c) = object ["i" .= c]

events :: MonadIO m => C.ConduitT () Counter m ()
events =
  C.yieldMany [1 ..]
    .| C.mapMC (\i -> do liftIO $ threadDelay (1000 * 1000); return i)
    .| C.mapC Counter

data Topic m a = Topic {chan :: TChan a, write :: C.ConduitT () C.Void m ()}

topic :: MonadIO m => C.ConduitT () a m () -> m (Topic m a)
topic src = do
  ch <- liftIO newBroadcastTChanIO
  let write = src .| C.mapM_C (liftIO . atomically . writeTChan ch)
  return $ Topic {chan = ch, write = write}

subscribe :: MonadIO m => Topic m a -> m (C.ConduitT () a m ())
subscribe (Topic {chan = ch}) = do
  newCh <- liftIO $ atomically $ dupTChan ch
  return (C.repeatMC $ liftIO . atomically $ readTChan newCh)

run :: MonadIO m => Topic m a -> m ()
run Topic {write = e} = C.runConduit e

data App = App

mkYesod
  "App"
  [parseRoutes|
    /sse ServerSentEventsR GET
|]

instance Yesod App where
  makeSessionBackend _ = return Nothing
  shouldLogIO _ _ _ = return True

getServerSentEventsR :: Handler TypedContent
getServerSentEventsR = repEventSource magic
  where
    magic _ = events .| C.mapC ev
    ev d =
      ServerEvent
        { eventName = Just $ B.putStringUtf8 "Counter",
          eventId = Just $ B.putStringUtf8 "1",
          eventData = [B.putStringUtf8 $ L.unpack (encode d)]
        }

-- getServerSentEventsR :: Handler Html
-- getServerSentEventsR = defaultLayout [whamlet|Hello World!|]

main :: IO ()
main = do
  -- t <- topic events
  -- _ <- forkIO $ run t
  warp 3000 App

-- main = C.runConduit $ C.yieldMany [1..10 :: Int] `C.fuse` C.mapMC (\i -> do { liftIO $ threadDelay (1000 * 1000); return i }) `C.fuse` C.mapM_C print
-- errorHandler NotFound = undefined
-- errorHandler other = defaultErrorHandler other

-- getHomeR :: Handler Html
-- getHomeR = defaultLayout [whamlet|<a href=@{Page1R}>Go to page 1 !|]

-- getPage1R = defaultLayout [whamlet|<a href=@{Page2R}>Go to page 2 !|]

-- getPage2R = defaultLayout [whamlet|<a href=@{HomeR}>Go to home !|]

-- getFactR :: Natural -> Handler T.Text -- text/plain
-- getFactR _ = return $ T.pack "This will be a factorial pretty soon"