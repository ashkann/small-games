{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

import Conduit
import Conduit qualified as C
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Data.Aeson
import Data.Binary.Builder qualified as B
import Data.ByteString.Lazy.Char8 qualified as L
import Network.Wai.EventSource.EventStream
import Yesod.Core
import Yesod.EventSource

-- newtype Natural = Natural Int deriving (Eq, Show, Read)

-- instance PathPiece Natural where
--   toPathPiece (Natural i) = T.pack $ show i
--   fromPathPiece s =
--     case reads $ T.unpack s of
--       (i, "") : _
--         | i < 1 -> Nothing
--         | otherwise -> Just $ Natural i
--       _ -> Nothing

newtype Counter = Counter Int

instance ToJSON Counter where
  toJSON (Counter c) = object ["i" .= c]

events :: MonadIO m => C.ConduitT () Counter m ()
events =
  C.yieldMany [1 ..]
    .| C.mapMC (\i -> do liftIO $ threadDelay (1000 * 1000); return i)
    .| C.mapC Counter

newtype App = App {ch :: TChan Counter}

mkYesod
  "App"
  [parseRoutes|
    /sse ServerSentEventsR GET
|]

instance Yesod App where
  makeSessionBackend _ = return Nothing
  shouldLogIO _ _ _ = return True

getServerSentEventsR :: Handler TypedContent
getServerSentEventsR = do
  App {ch = source} <- getYesod
  chann <- liftIO $ atomically $ dupTChan source
  let readCh = C.repeatMC (liftIO . atomically $ readTChan chann)
  repEventSource (\_ -> readCh .| C.mapC toEvent)
  where
    toEvent a =
      ServerEvent
        { eventName = Just $ B.putStringUtf8 "Counter",
          eventId = Nothing, -- Just $ B.putStringUtf8 "1",
          eventData = [B.putStringUtf8 $ L.unpack (encode a)]
        }

main :: IO ()
main = do
  ch <- liftIO newBroadcastTChanIO
  let write = events .| C.mapM_C (liftIO . atomically . writeTChan ch)
  _ <- forkIO $ C.runConduit write
  warp 3000 $ App ch