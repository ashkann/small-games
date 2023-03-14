{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Counter where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad (void)
import Data.Functor ((<&>))
import Room qualified as R
import Yesod.Core
import Prelude hiding (id, log, read)

data Output = NoOp | Counter Int

data Input = Reset Int | Slower | Faster

counterGame :: MonadIO m => Int -> Int -> R.Game m Input Output
counterGame c0 s0 = R.createGame create
  where
    create ichan ochan = void $ liftIO . forkIO $ go c0 s0
      where
        go c s = do
          _ <- atomically $ writeTChan ochan (Counter c)
          (c', s') <- atomically (tryReadTChan ichan) <&> maybe (c + 1, s) process
          _ <- let delay = (11 - s') * 500 * 1000 in threadDelay delay
          go c' s'
          where
            process Faster = (c + 1, s + 1)
            process Slower = (c + 1, s - 1)
            process (Reset c'') = (c'', s)

instance ToJSON Output where
  toJSON (Counter c) = object ["i" .= c]
  toJSON NoOp = object []