{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

import Brick
import Graphics.Vty qualified as V

newtype Counter = Counter Int

main :: IO ()
main = do
  let app =
        App
          { appDraw = \case
              Just i -> [str $ "Connected " ++ show i]
              Nothing -> [str "Not Connected"],
            appChooseCursor = \_ _ -> Nothing,
            appHandleEvent = \case
              VtyEvent (V.EvKey V.KEsc []) -> halt
              VtyEvent (V.EvKey V.KEnter []) -> halt
              AppEvent (Counter i) -> put $ Just i
              _ -> return (),
            appStartEvent = return (),
            appAttrMap = const $ attrMap V.defAttr []
          } ::
          App (Maybe Int) Counter ()
      initialState = Nothing :: Maybe Int
  _ <- defaultMain app initialState
  return ()