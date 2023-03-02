{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

import Conduit
import Conduit qualified as C
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Data.Aeson
import Data.Binary.Builder qualified as B
import Data.ByteString.Lazy.Char8 qualified as L
import Data.Map qualified as M
import Data.Text qualified as T
import Network.Wai.EventSource.EventStream
import Session (InMemory)
import Session qualified as S
import Yesod.Core
import Yesod.EventSource

-- import TicTacToe

-- import Session (SessionManager(create))

newtype GameId = GameId Int deriving (Eq, Show, Read)

instance PathPiece GameId where
  toPathPiece (GameId i) = T.pack $ show i
  fromPathPiece s =
    case reads $ T.unpack s of
      (i, "") : _
        | i < 1 -> Nothing
        | otherwise -> Just $ GameId i
      _ -> Nothing

newtype Counter = Counter Int

newtype Game m o = Game {events :: C.ConduitT () o m ()}

createGame :: MonadIO m => m (Game m Counter)
createGame =
  return
    Game
      { events =
          C.yieldMany [1 ..]
            .| C.mapMC (\i -> do liftIO $ threadDelay (2 * 1000 * 1000); return i)
            .| C.mapC Counter
      }

-- startGame :: Game m Counter -> m ()
-- startGame (Game events) = _

instance ToJSON Counter where
  toJSON (Counter c) = object ["i" .= c]

data Room m o = Room {game :: Game m o, chan :: TChan o}

newtype App = App {rooms :: TVar (M.Map Int (Room (HandlerFor App) Counter))}

mkYesod
  "App"
  [parseRoutes|
    /create CreateGameR POST
    /join/#GameId JoinGameR POST
|]

instance Yesod App where
  makeSessionBackend _ = return Nothing
  shouldLogIO _ _ _ = return True

withRooms :: InMemory Int (Room Handler Counter) a -> Handler a
withRooms act = do
  App {rooms = r} <- getYesod
  liftIO . atomically $ S.run act r

instance S.SessionManager Handler GameId (Room Handler Counter) where
  create room = GameId <$> withRooms (S.create room)
  write (GameId id) room = withRooms (S.write id room)
  update f (GameId id) = withRooms (S.update f id)
  delete (GameId id) = withRooms (S.delete id)
  read (GameId id) = withRooms (S.read id)

toServerEvent :: ToJSON a => a -> ServerEvent
toServerEvent a =
  ServerEvent
    { eventName = Just $ B.putStringUtf8 "Counter",
      eventId = Nothing, -- Just $ B.putStringUtf8 "1",
      eventData = [B.putStringUtf8 $ L.unpack (encode a)]
    }

join :: ToJSON b => Room m b -> HandlerFor site TypedContent
join Room {chan = ch} = do
  chann <- liftIO $ atomically $ dupTChan ch
  let readCh = C.repeatMC (liftIO . atomically $ readTChan chann)
  repEventSource (\_ -> readCh .| C.mapC toServerEvent)    

postCreateGameR :: Handler TypedContent
postCreateGameR = do
  g@(Game events) <- createGame
  ch <- liftIO newBroadcastTChanIO
  let write = events .| C.mapM_C (liftIO . atomically . writeTChan ch)
  let room = Room g ch
  _ <- S.create room
  join room <* forkHandler ($logError . T.pack . show) (C.runConduit write)

postJoinGameR :: GameId -> Handler TypedContent
postJoinGameR id = join =<< S.read id

main :: IO ()
main = do
  rooms <- newTVarIO M.empty
  warp 3000 $ App rooms