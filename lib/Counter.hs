{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Counter where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Yesod.Core
import Prelude hiding (id, log, read)
import Game qualified as G
import Prelude hiding (id, read, log)

data Output = NoOp | Counter Int

data Input = Reset Int | Slower | Faster

counterGame :: MonadIO m => G.Game m (Int, Int) Input Output
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
   in G.Game {G.createGame = f}

instance ToJSON Output where
  toJSON (Counter c) = object ["i" .= c]
  toJSON NoOp = object []   