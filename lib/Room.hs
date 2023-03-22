{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Room
  ( Room,
    MonadRoom (..),
    RoomT (..),
  )
where

import Conduit (ConduitT, repeatMC)
import Control.Concurrent.STM
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader (ask), ReaderT (..))
import Game qualified as G
import Prelude hiding (read)

data Room i o = Room {ichan :: TChan i, ochan :: TChan o}

newtype RoomT m i o a = RoomT {runRoomT :: ReaderT (Room i o) m a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader (Room i o)
    )

instance MonadIO m => G.Write (RoomT m i o) o where
  write o = do
    Room {ochan = ochan} <- ask
    liftIO . atomically $ writeTChan ochan o

instance MonadIO m => G.Read (RoomT m i o) i where
  read = do
    Room {ichan = ichan} <- ask
    liftIO . atomically $ readTChan ichan

instance MonadIO m => G.TryRead (RoomT m i o) i where
  tryRead = do
    Room {ichan = ichan} <- ask
    liftIO . atomically $ tryReadTChan ichan

class MonadRoom m where
  create :: m (Room i o)
  write :: Room i o -> i -> m ()
  join :: Room i o -> m (ConduitT () o m ())

instance MonadIO m => MonadRoom m where
  create = do
    ochan <- liftIO newBroadcastTChanIO
    ichan <- liftIO newTChanIO
    return $ Room {ichan = ichan, ochan = ochan}

  write Room {ichan = ichan} i = liftIO . atomically $ writeTChan ichan i

  join Room {ochan = ochan} = do
    ch <- liftIO . atomically $ dupTChan ochan
    return $ repeatMC (liftIO . atomically $ readTChan ch)