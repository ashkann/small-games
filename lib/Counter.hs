{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Counter where

import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Data.Functor ((<&>))
import Game qualified as G
import Yesod.Core
import Prelude hiding (id, log, read)

data Output = NoOp | Counter Int

data Input = Reset Int | Slower | Faster

instance ToJSON Output where
  toJSON (Counter c) = object ["i" .= c]
  toJSON NoOp = object []

mkGame :: (MonadIO m, G.TryRead m Input, G.Write m Output) => Int -> Int -> m ()
mkGame c0 s0 = void $ go c0 s0
  where
    go c s = do
      _ <- G.write (Counter c)
      (c', s') <- G.tryRead <&> maybe (c + 1, s) process
      _ <- let delay = (11 - s') * 500 * 1000 in liftIO . threadDelay $ delay
      go c' s'
      where
        process Faster = (c + 1, s + 1)
        process Slower = (c + 1, s - 1)
        process (Reset c'') = (c'', s)