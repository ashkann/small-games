module Room
  ( Room,
    Game,
    createRoom,
    writeRoom,
    joinRoom,
    hostGame,
    createGame
  )
where

import Conduit (ConduitT, repeatMC)
import Control.Concurrent.STM
import Control.Monad.IO.Class (MonadIO, liftIO)

newtype Game m i o = Game {create :: TChan i -> TChan o -> m ()}

data Room i o = Room {ichan :: TChan i, ochan :: TChan o}

createRoom :: MonadIO m => m (Room i o)
createRoom = do
  ochan <- liftIO newBroadcastTChanIO
  ichan <- liftIO newTChanIO
  return $ Room {ichan = ichan, ochan = ochan}

writeRoom :: MonadIO m => Room i o -> i -> m ()
writeRoom Room {ichan = ichan} i = liftIO . atomically $ writeTChan ichan i

joinRoom :: MonadIO m => Room i o -> m (ConduitT () o m ())
joinRoom Room {ochan = ochan} = do
  ch <- liftIO . atomically $ dupTChan ochan
  return $ repeatMC (liftIO . atomically $ readTChan ch)

hostGame :: Room i o -> Game m i o -> m ()
hostGame Room {ichan = ichan, ochan = ochan} Game {create = create} = create ichan ochan

createGame :: (TChan i -> TChan o -> m ()) -> Game m i o
createGame f = Game {create = f}