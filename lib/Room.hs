{-# LANGUAGE  FlexibleInstances  #-}
{-# LANGUAGE  MultiParamTypeClasses  #-}
{-# LANGUAGE  UndecidableInstances  #-}

module Room
  ( Room,
    Game,
    MonadRoom(..),
    createGame,
  )
where

import Conduit (ConduitT, repeatMC)
import Control.Concurrent.STM
import Control.Monad.IO.Class (MonadIO, liftIO)

newtype Game m i o = Game {run :: TChan i -> TChan o -> m ()}

data Room i o = Room {ichan :: TChan i, ochan :: TChan o}

class MonadRoom m where
  create :: m (Room i o)
  write :: Room i o -> i -> m ()
  join :: Room i o -> m (ConduitT () o m ())
  host :: Room i o -> Game m i o -> m ()

instance MonadIO m => MonadRoom m where
  create = do
    ochan <- liftIO newBroadcastTChanIO
    ichan <- liftIO newTChanIO
    return $ Room {ichan = ichan, ochan = ochan}

  write Room {ichan = ichan} i = liftIO . atomically $ writeTChan ichan i

  join Room {ochan = ochan} = do
    ch <- liftIO . atomically $ dupTChan ochan
    return $ repeatMC (liftIO . atomically $ readTChan ch)

  host Room {ichan = ichan, ochan = ochan} Game {run = run} = run ichan ochan

createGame :: (TChan i -> TChan o -> m ()) -> Game m i o
createGame f = Game {run = f}