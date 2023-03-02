{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Session (SessionManager (..), InMemory (..)) where

import Control.Concurrent.STM (STM, TVar, modifyTVar, readTVar, throwSTM)
import Data.Map qualified as M
import Prelude hiding (id, lookup)
import Control.Exception (SomeException, Exception)

class SessionManager m k s | m -> k s where
  create :: s -> m k
  read :: k -> m s
  write :: k -> s -> m ()
  update :: (s -> s) -> k -> m ()
  delete :: k -> m ()

newtype InMemory id s a = InMemory {run :: TVar (M.Map id s) -> STM a}

modify :: (M.Map id s -> M.Map id s) -> InMemory id s ()
modify f = InMemory $ \tvar -> do modifyTVar tvar f

newtype MyException = MyException String deriving Show

instance Exception MyException

instance SessionManager (InMemory Int a) Int a where
  create s = InMemory $ \tvar -> do
    id <- (+ 1) . M.size <$> readTVar tvar
    modifyTVar tvar (M.insert id s)
    return id

  read id = InMemory $ \tvar -> do
    s <- M.lookup id <$> readTVar tvar
    case s of
      Just s' -> return s'
      Nothing -> throwSTM $ MyException ("No such element" ++ show id)

  write id s = modify $ M.insert id s
  update f id = modify $ M.update (Just . f) id
  delete id = modify $ M.delete id