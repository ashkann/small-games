{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Brick
import Brick.BChan (newBChan, writeBChan)
import Brick.Widgets.Center
import Conduit
import Control.Concurrent (forkIO)
import Control.Monad
import Data.Aeson
import Data.Attoparsec.ByteString qualified as P
import Data.Binary.Builder
import Data.ByteString (ByteString)
import Graphics.Vty qualified as V
import Network.HTTP.Client.Conduit (Response (responseBody))
import Network.HTTP.Simple
import Network.Wai.EventSource

newtype Counter = Counter Int

instance FromJSON Counter where
  parseJSON (Object v) = Counter <$> v .: "i"
  parseJSON _ = fail "Counldn't parse"

draw :: Maybe Int -> [Widget ()]
draw = \case
  Just i -> go $ show i
  Nothing -> go "Not connected"
  where
    go s1 = [vBox [header, body s1, footer] :: Widget ()]
    header = vLimit 1 $ withAttr (attrName "header") $ center $ str "Small Games"
    body s = padBottom Max $ strWrap s
    footer = vLimit 1 $ withAttr (attrName "footer") $ strWrap "Esc: Quit"

globalDefault :: V.Attr
globalDefault = fg V.white

theMap =
  attrMap
    globalDefault
    [ (attrName "footer", V.black `on` V.white),
      (attrName "header", V.black `on` V.white)
    ]

instance Show ServerEvent where
  show (ServerEvent {eventName = Just n, eventData = [d]}) = "name=[" ++ show n ++ "], data=[" ++ show d ++ "]"
  show _ = "Oops !"

main :: IO ()
main = do
  chan <- newBChan 10
  _ <- forkIO $ runConduitRes $ httpSource "http://localhost:3000/sse" $ publish chan . responseBody
  let app =
        App
          { appDraw = draw,
            appChooseCursor = \_ _ -> Nothing,
            appHandleEvent = \case
              VtyEvent (V.EvKey V.KEsc []) -> halt
              VtyEvent (V.EvKey V.KEnter []) -> halt
              AppEvent (Counter i) -> put $ Just i
              _ -> return (),
            appStartEvent = return (),
            appAttrMap = const theMap
          }
      initialState = Nothing
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  _ <- customMain initialVty buildVty (Just chan) app initialState
  return ()
  where
    publish chan x =
      x
        .| concatMapC toServerEvent
        .| concatMapC toAppEvent
        .| mapM_C (liftIO . writeBChan chan)

    toServerEvent :: ByteString -> Either String ServerEvent
    toServerEvent = P.parseOnly p
      where
        p = do
          let newLine = toEnum (fromEnum '\n')
          _ <- P.string "event:"
          eventName <- P.takeWhile1 (/= newLine)
          _ <- P.satisfy (== newLine)
          _ <- P.string "data:"
          eventData <- P.takeWhile1 (/= newLine)
          replicateM_ 2 $ P.satisfy (== newLine)
          return
            ServerEvent
              { eventData = [fromByteString eventData],
                eventName = Just $ fromByteString eventName,
                eventId = Nothing
              }

    toAppEvent :: ServerEvent -> Either String Counter
    toAppEvent (ServerEvent {eventData = [d], eventName = Just _}) =
      case decode $ toLazyByteString d of
        Just x -> Right x
        Nothing -> Left "Can't decode"
    toAppEvent _ = Left "Not a valid server event"