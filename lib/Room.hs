{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Room
  ( Room,
    MonadRoom (..),
    RoomT (..),
  )
where

import Conduit (ConduitT, repeatMC)
import Control.Concurrent.STM
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT (..))
import Game qualified as G
import Prelude hiding (read)

data Room i o = Room {ichan :: TChan i, ochan :: TChan o}

newtype RoomT i o a = RoomT {runRoomT :: ReaderT (Room i o) IO a} deriving (Functor, Applicative, Monad, MonadIO)

instance G.Write (RoomT i o) o where
  write o = RoomT . ReaderT $ \Room {ochan = ochan} -> atomically $ writeTChan ochan o

instance G.Read (RoomT i o) i where
  read = RoomT . ReaderT $ \Room {ichan = ichan} -> atomically $ readTChan ichan

instance G.TryRead (RoomT i o) i where
  tryRead = RoomT . ReaderT $ \Room {ichan = ichan} -> atomically $ tryReadTChan ichan

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