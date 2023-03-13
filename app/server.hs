{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

import Conduit ((.|))
import Conduit qualified as C
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad (forever, void)
import Data.Aeson
import Data.Binary.Builder qualified as B
import Data.ByteString.Lazy.Char8 qualified as L
import Data.Conduit.Combinators qualified as C
import Data.Map qualified as M
import Data.Text qualified as T
import Host qualified as H
import Network.Wai.EventSource.EventStream
import Yesod.Core
import Yesod.EventSource
import Prelude hiding (id, read, log)

-- Game
type Callback m o = o -> m ()

type Submit m i = i -> m ()

newtype Game m g i o = Game {createGame :: g -> Callback IO o -> m (Submit m i)}

-- The Counter game
data Output = NoOp | Counter Int

data Input = Reset Int | Slower | Faster

counterGame :: forall m. MonadIO m => Game m (Int, Int) Input Output
counterGame =
  let f (c, s) callbackIO = do
        ichan <- liftIO newTChanIO
        _ <- liftIO . forkIO $ go c s ichan callbackIO -- Run the game loop in background
        return $ submit ichan

      -- parse the input, compute the new counter and speed, send, wait then repeat
      go c s ichan callback = do
        maybeInput <- atomically $ tryReadTChan ichan
        let (c', s') = case maybeInput of
              Just Faster -> (c + 1, s + 1)
              Just Slower -> (c + 1, s - 1)
              Just (Reset c'') -> (c'', s)
              Nothing -> (c + 1, s)
        _ <- callback (Counter c')
        _ <- let delay = (11 - s') * 500 * 1000 in threadDelay delay
        go c' s' ichan callback

      submit chan i = liftIO . atomically $ writeTChan chan i
   in Game {createGame = f}

-- Room
type Subscribe m o = Callback IO o -> m ()

data Room m i o = Room {submit :: Submit m i, subscribe :: Subscribe m o}

createRoom :: MonadIO m => Game m g i o -> g -> m (Room m i o)
createRoom Game {createGame = create} g = do
  out <- liftIO . atomically $ newBroadcastTChan
  let write o = atomically $ writeTChan out o
  submit <- create g write
  let subscribe cb = do
        ch <- liftIO . atomically $ dupTChan out
        let go = forever $ cb =<< atomically (readTChan ch)
        void $ liftIO . forkIO $ go

  return $ Room {submit = submit, subscribe = subscribe}

subscribeC :: MonadIO m => Room m i o -> m (C.ConduitT () o m ())
subscribeC Room {subscribe = subscribe} = do
  ch <- liftIO newTChanIO
  _ <- subscribe $ atomically . writeTChan ch
  return $ C.repeatM (liftIO . atomically $ readTChan ch)

newtype GameId = GameId Int deriving (Eq, Show, Read)

instance PathPiece GameId where
  toPathPiece (GameId i) = T.pack $ show i
  fromPathPiece s =
    case reads $ T.unpack s of
      (i, "") : _
        | i < 1 -> Nothing
        | otherwise -> Just $ GameId i
      _ -> Nothing

-- createGame2 :: C.ConduitT () Input m () -> C.ConduitT Output () m r -> m r
-- createGame2 is os = undefined

instance ToJSON Output where
  toJSON (Counter c) = object ["i" .= c]
  toJSON NoOp = object []

newtype App = App {rooms :: TVar (M.Map Int (Room (HandlerFor App) Input Output))}

-- newtype App = App Int

mkYesod
  "App"
  [parseRoutes|
    /create CreateGameR POST
    /join/#GameId JoinGameR POST
|]

app :: H.InMemory Int (Room Handler Input Output) a -> Handler a
app (H.InMemory run) = getYesod >>= (liftIO . atomically . run . rooms)

log :: String -> Handler ()
log msg = $(logDebug) (T.pack msg)

instance H.Host Handler GameId (Room Handler Input Output) where
  create r = GameId <$> (app . H.create) r
  read (GameId id) = app $ H.read id
  write (GameId id) r = app $ H.write id r
  update f (GameId id) = app $ H.update f id
  delete (GameId id) = app $ H.delete id

instance Yesod App where
  makeSessionBackend _ = return Nothing
  shouldLogIO _ _ _ = return True

toServerEvent :: ToJSON a => a -> ServerEvent
toServerEvent a =
  ServerEvent
    { eventName = Just $ B.putStringUtf8 "Counter",
      eventId = Nothing, -- Just $ B.putStringUtf8 "1",
      eventData = [B.putStringUtf8 $ L.unpack (encode a)]
    }

join :: ToJSON o => Room Handler i o -> Handler TypedContent
join room = do
  c <- subscribeC room
  repEventSource $ const (c .| C.mapC toServerEvent)

postCreateGameR :: Handler TypedContent
postCreateGameR = do
  room <- createRoom counterGame (0, 1)
  id <- H.create room
  _ <- log $ "Created game on room " ++ show id
  join room

-- postJoinGameR :: GameId -> Handler TypedContent
-- postJoinGameR id = undefined

postJoinGameR :: GameId -> Handler TypedContent
postJoinGameR id = do
  room <- H.read id
  _ <- log $ "Joined room " ++ show id
  join room

main :: IO ()
main = do
  rooms <- newTVarIO M.empty
  warp 3000 $ App rooms

-- main = putStrLn " Okay"