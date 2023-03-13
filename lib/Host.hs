{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE UndecidableInstances #-}

module Host(Host(..), InMemory(..)) where

import Control.Concurrent.STM
import Control.Exception qualified as Ex
import Data.Data (Typeable)
import Data.Map qualified as M
import Prelude hiding (id, lookup, read)

newtype NotFound k = NotFound k deriving (Show)

instance (Typeable k, Show k) => Ex.Exception (NotFound k)

class Host m k s | m -> k s where
  create :: s -> m k
  read :: k -> m s
  write :: k -> s -> m ()
  update :: (s -> s) -> k -> m ()
  delete :: k -> m ()

newtype InMemory k s a = InMemory {run :: TVar (M.Map k s) -> STM a}

modify :: (M.Map k s -> M.Map k s) -> InMemory k s ()
modify f = InMemory $ \tvar -> do modifyTVar tvar f

instance Host (InMemory Int a) Int a where
  create s = InMemory $ \tvar -> do
    id <- succ . M.size <$> readTVar tvar
    modifyTVar tvar (M.insert id s)
    return id

  read id = InMemory $ \tvar -> do
    s <- M.lookup id <$> readTVar tvar
    maybe err return s
    where
      err = throwSTM $ NotFound id

  write id s = modify $ M.insert id s
  update f id = modify $ M.update (Just . f) id
  delete id = modify $ M.delete id