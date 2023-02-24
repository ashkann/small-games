{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Resource (runResourceT)
import           Data.Conduit                 (($$))
import qualified Data.Conduit.Binary          as CB
import qualified Data.Conduit.List            as CL
import           Network.HTTP.Simple
import           System.IO                    (stdout)

main :: IO ()
main =
    runResourceT
        $ a
       $$ CB.sinkHandle stdout
  where
    a = httpSource "http://httpbin.org/robots.txt" getSrc
    getSrc res = do
        liftIO $ print (getResponseStatus res, getResponseHeaders res)
        getResponseBody res