module Game where

import Conduit (ConduitT, repeatMC)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad (forever, void)
import Yesod.Core
import Prelude hiding (id, log, read)

-- Game
type Callback m o = o -> m ()

type Submit m i = i -> m ()

newtype Game m g i o = Game {createGame :: g -> Callback IO o -> m (Submit m i)}

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

subscribeC :: MonadIO m => Room m i o -> m (ConduitT () o m ())
subscribeC Room {subscribe = subscribe} = do
  ch <- liftIO newTChanIO
  _ <- subscribe $ atomically . writeTChan ch
  return $ repeatMC (liftIO . atomically $ readTChan ch)