module Game
  ( Game (..),
    Room (..),
    createRoom,
  )
where

import Conduit (ConduitT, repeatMC)
import Control.Concurrent.STM
import Control.Monad.IO.Class (MonadIO, liftIO)

newtype Game m i o = Game {create :: TChan i -> TChan o -> m ()}

data Room m i o = Room {startGame :: m (), input :: i -> m (), output :: m (ConduitT () o m ())}

createRoom :: MonadIO m => Game m i o -> m (Room m i o)
createRoom Game {create = create} = do
  ochan <- liftIO newBroadcastTChanIO
  ichan <- liftIO newTChanIO
  let g = create ichan ochan
  let output = do
        ch <- liftIO . atomically $ dupTChan ochan
        return $ repeatMC (liftIO . atomically $ readTChan ch)
  let input i = liftIO . atomically $ writeTChan ichan i
  return $ Room {input = input, output = output, startGame = g}