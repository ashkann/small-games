{-# LANGUAGE FunctionalDependencies #-}

module Session () where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State (runState)
import Control.Monad.State qualified as ST
import Data.Map qualified as M
import GHC.IO.Handle (Handle, hClose, hPutStr)
import System.IO (openTempFile)
import Prelude hiding (id, lookup)

class SessionManager m id session | m -> session id where
  create :: session -> m id

--   lookup :: id -> m (Maybe session)
--   update :: (session -> session) -> id -> m ()
--   delete :: id -> m ()

type InMemoryBackend = ST.State (M.Map Int String)

instance SessionManager InMemoryBackend Int String where
  create s = do
    newId <- ST.gets $ (+ 1) . M.size
    _ <- ST.modify $ M.insert newId s
    return newId

--   lookup id = ST.gets $ M.lookup id

--   delete id = ST.modify $ M.delete id

--   update f id = do
--     s <- fmap f <$> lookup id
--     case s of
--       Just s' -> ST.modify $ M.insert id s'
--       Nothing -> return ()

-- x :: ST.State InMemoryBackend (Int, Int)
x :: InMemoryBackend (Int, Int) = do
  ix1 <- create "Ashkan"
  ix2 <- create "Yasaman"
  return (ix1, ix2)

((i1, i2), backend) = runState x M.empty

data FS = FS {root :: FilePath, template :: String}

newtype FileSystem a = FileSystem (FS -> IO a)

runFS :: FS -> FileSystem a -> IO a
runFS r (FileSystem m) = m r

instance Functor FileSystem where
  fmap f (FileSystem m) = FileSystem $ fmap f . m

instance Applicative FileSystem where
  pure = FileSystem . const . pure
  FileSystem mf <*> FileSystem ma = FileSystem $ \r -> mf r <*> ma r

instance Monad FileSystem where
  FileSystem ma >>= f = FileSystem $ \r -> do
    a <- ma r
    let FileSystem f' = f a
    f' r

instance SessionManager FileSystem FilePath String where
  create s = FileSystem $ \(FS r t) -> do
    (p, h) <- openTempFile r t
    _ <- hPutStr h s
    _ <- hClose h
    return p

--   lookup id = liftIO $ fsRead id

--   update f id = undefined
--   delete id = undefined

-- example :: SessionManager m MemorySessionManager => m -> ()
-- example = do
--     id <- create

-- class Magic m where
--     doMagic :: m ()

-- newtype A a = A { runA :: IO a }
-- -- instance MonadIO A where
-- --     liftIO = A

-- instance Magic A where
--     doMagic = do
--         _ <- _
--         return ()