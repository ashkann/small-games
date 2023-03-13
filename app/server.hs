{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

import Conduit ((.|))
import Conduit qualified as C
import Control.Concurrent.STM
import Data.Aeson
import Data.Binary.Builder qualified as B
import Data.ByteString.Lazy.Char8 qualified as L
import Data.Map qualified as M
import Data.Text qualified as T
import Host qualified as H
import Game qualified as G
import Network.Wai.EventSource.EventStream
import Yesod.Core
import Yesod.EventSource
import Prelude hiding (id, read, log)
import Counter qualified as Cn

newtype GameId = GameId Int deriving (Eq, Show, Read)

instance PathPiece GameId where
  toPathPiece (GameId i) = T.pack $ show i
  fromPathPiece s =
    case reads $ T.unpack s of
      (i, "") : _
        | i < 1 -> Nothing
        | otherwise -> Just $ GameId i
      _ -> Nothing

newtype App = App {rooms :: TVar (M.Map Int (G.Room (HandlerFor App) Cn.Input Cn.Output))}

-- newtype App = App Int

mkYesod
  "App"
  [parseRoutes|
    /create CreateGameR POST
    /join/#GameId JoinGameR POST
|]

app :: H.InMemory Int (G.Room Handler Cn.Input Cn.Output) a -> Handler a
app (H.InMemory run) = getYesod >>= (liftIO . atomically . run . rooms)

log :: String -> Handler ()
log msg = $(logDebug) (T.pack msg)

instance H.Host Handler GameId (G.Room Handler Cn.Input Cn.Output) where
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

join :: ToJSON o => G.Room Handler i o -> Handler TypedContent
join room = do
  c <- G.subscribeC room
  repEventSource $ const (c .| C.mapC toServerEvent)

postCreateGameR :: Handler TypedContent
postCreateGameR = do
  room <- G.createRoom Cn.counterGame (0, 1)
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