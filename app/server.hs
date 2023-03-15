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
import Counter qualified as Cn
import Data.Aeson
import Data.Binary.Builder qualified as B
import Data.ByteString.Lazy.Char8 qualified as L
import Data.Map qualified as M
import Data.Text qualified as T
import Host qualified as H
import Network.Wai.EventSource.EventStream
import Room qualified as R
import Yesod.Core
import Yesod.EventSource
import Prelude hiding (id, log, read)
import TicTacToe qualified as T3

newtype GameId = GameId Int deriving (Eq, Show, Read)

instance PathPiece GameId where
  toPathPiece (GameId i) = T.pack $ show i
  fromPathPiece s =
    case reads $ T.unpack s of
      (i, "") : _
        | i < 1 -> Nothing
        | otherwise -> Just $ GameId i
      _ -> Nothing

newtype App = App {rooms :: TVar (M.Map Int (R.Room Cn.Input Cn.Output))}

mkYesod
  "App"
  [parseRoutes|
    /create CreateGameR POST
    /create2 CreateGame2R POST
    /#GameId/join JoinGameR POST
    /#GameId/faster FasterR POST
    /#GameId/slower SlowerR POST
    /#GameId/reset ResetR POST
|]

app :: H.InMemory Int (R.Room Cn.Input Cn.Output) a -> Handler a
app (H.InMemory run) = getYesod >>= (liftIO . atomically . run . rooms)

log :: String -> Handler ()
log msg = $(logDebug) (T.pack msg)

instance H.Host Handler GameId (R.Room Cn.Input Cn.Output) where
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

join :: (ToJSON o, MonadIO m) => R.Room i o -> m (C.ConduitT () ServerEvent m ())
join room = (.| C.mapC toServerEvent) <$> R.join room

postCreateGameR :: Handler TypedContent
postCreateGameR = do
  room <- R.create
  id <- H.create room
  out <- join room
  _ <- R.host room $ Cn.counterGame 0 1
  _ <- log $ "Created room " ++ show id
  repEventSource . const $ out

postCreateGame2R :: Handler TypedContent
postCreateGame2R = do
  room <- R.create
  id <- H.create room
  out <- join room
  _ <- R.host room $ Cn.counterGame 0 1
  _ <- log $ "Created room " ++ show id
  repEventSource . const $ out

postFasterR :: GameId -> Handler TypedContent
postFasterR id = do
  room <- H.read id
  _ <- R.write room Cn.Faster
  _ <- log $ "Faster " ++ show id
  sendResponse ()

postSlowerR :: GameId -> Handler TypedContent
postSlowerR id = do
  room <- H.read id
  _ <- R.write room Cn.Slower
  _ <- log $ "Slower " ++ show id
  sendResponse ()

postResetR :: GameId -> Handler TypedContent
postResetR id = do
  room <- H.read id
  _ <- R.write room $ Cn.Reset 0
  _ <- log $ "Slower " ++ show id
  sendResponse ()

postJoinGameR :: GameId -> Handler TypedContent
postJoinGameR id = do
  room <- H.read id
  _ <- log $ "Joined room " ++ show id
  join room >>= repEventSource . const

main :: IO ()
main = do
  rooms <- newTVarIO M.empty
  warp 3000 $ App rooms