{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

import Data.Aeson
import Data.Text qualified as T
import Data.Binary.Builder qualified as B
import Conduit qualified as C
import Yesod.Core
import Control.Concurrent (threadDelay)

newtype Natural = Natural Int deriving (Eq, Show, Read)

instance PathPiece Natural where
  toPathPiece (Natural i) = T.pack $ show i
  fromPathPiece s =
    case reads $ T.unpack s of
      (i, "") : _
        | i < 1 -> Nothing
        | otherwise -> Just $ Natural i
      _ -> Nothing

data App = App

data Person = Person {name :: T.Text, age :: Int}

instance ToJSON Person where
  toJSON Person {..} =
    object
      [ "name" .= name,
        "age" .= age
      ]

mkYesod
  "App"
  [parseRoutes|
    /sse ServerSentEventsR GET
|]

instance Yesod App where
  makeSessionBackend _ = return Nothing
  shouldLogIO App _ _ = return True

-- errorHandler NotFound = undefined
-- errorHandler other = defaultErrorHandler other

-- getHomeR :: Handler Html
-- getHomeR = defaultLayout [whamlet|<a href=@{Page1R}>Go to page 1 !|]

-- getPage1R = defaultLayout [whamlet|<a href=@{Page2R}>Go to page 2 !|]

-- getPage2R = defaultLayout [whamlet|<a href=@{HomeR}>Go to home !|]

-- getFactR :: Natural -> Handler T.Text -- text/plain
-- getFactR _ = return $ T.pack "This will be a factorial pretty soon"

getServerSentEventsR :: Handler TypedContent
getServerSentEventsR = respondSource "text/event-stream" $ events `C.fuse` C.concatC
  where
    xs = C.yieldMany [1..] :: C.ConduitT () Int Handler ()
    ys = xs `C.fuse` C.mapMC (\i -> do { liftIO $ threadDelay (1000 * 1000); return i })
    zs = ys `C.fuse` C.mapC (\i -> "{\"i\": " ++ show i ++ "}")
    events = zs `C.fuse` C.mapC (\d -> [C.Chunk $ B.putStringUtf8 $ "data:" ++ d ++ "\n\n", C.Flush])

main :: IO ()
main = warp 3000 App
-- main = C.runConduit $ C.yieldMany [1..10 :: Int] `C.fuse` C.mapMC (\i -> do { liftIO $ threadDelay (1000 * 1000); return i }) `C.fuse` C.mapM_C print