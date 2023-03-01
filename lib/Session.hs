{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Session (SessionManager (..), InMemory (..)) where

import Control.Concurrent.STM (STM, TVar, modifyTVar, readTVar)
import Data.Map qualified as M
import Prelude hiding (id, lookup)

class SessionManager m k s | m -> k s where
  create :: s -> m k
  write :: k -> s -> m ()
  update :: (s -> s) -> k -> m ()
  delete :: k -> m ()

newtype InMemory id s a = InMemory {run :: TVar (M.Map id s) -> STM a}

modify :: (M.Map id s -> M.Map id s) -> InMemory id s ()
modify f = InMemory $ \tvar -> do modifyTVar tvar f

instance SessionManager (InMemory Int a) Int a where
  create s = InMemory $ \tvar -> do
    id <- (+ 1) . M.size <$> readTVar tvar
    modifyTVar tvar (M.insert id s)
    return id

  write id s = modify $ M.insert id s
  update f id = modify $ M.update (Just . f) id
  delete id = modify $ M.delete id