{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Game where

import Prelude hiding (read)

class Read m i where
  read :: m i -- blocking

class TryRead m i where
  tryRead :: m (Maybe i) -- non-blocking

class Write m o where
  write :: o -> m () -- write output