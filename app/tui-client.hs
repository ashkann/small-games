{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

import Brick
-- import Brick.Util (on, fg)
-- import Brick.AttrMap (attrMap, AttrMap, attrName)

import Brick.Widgets.Center
import Graphics.Vty

-- import Brick.Widgets.Center (hCenter)

newtype Counter = Counter Int

draw :: Maybe Int -> [Widget ()]
draw _ = [vBox [header, body, footer]]
  where
    header = vLimit 1 $ withAttr (attrName "header") $ center $ str "Small Games"
    body = padBottom Max $ strWrap "Not Connected"
    footer = vLimit 1 $ withAttr (attrName "footer") $ strWrap "Esc: Quit"

globalDefault :: Attr
globalDefault = fg white

theMap =
  attrMap
    globalDefault
    [ (attrName "footer", black `on` white),
      (attrName "header", black `on` white),
      (attrName "foundFgOnly", fg red),
      (attrName "general", yellow `on` black),
      ( attrName "general" <> attrName "specific",
        fg cyan
      )
      --   (attrName "linked", fg yellow `withURL` "http://www.google.com/")
    ]

main :: IO ()
main = do
  let app =
        App
          { appDraw = draw,
            appChooseCursor = \_ _ -> Nothing,
            appHandleEvent = \case
              VtyEvent (EvKey KEsc []) -> halt
              VtyEvent (EvKey KEnter []) -> halt
              AppEvent (Counter i) -> put $ Just i
              _ -> return (),
            appStartEvent = return (),
            appAttrMap = const theMap
          }
      initialState = Nothing
  _ <- defaultMain app initialState
  return ()