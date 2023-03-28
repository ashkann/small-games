{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (unless)
import SDL

main :: IO ()
main = do
  initializeAll
  window <- createWindow "Tic-Tac-Toe" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  appLoop renderer
  destroyWindow window
  where
    appLoop :: Renderer -> IO ()
    appLoop renderer = do
      events <- pollEvents
      rendererDrawColor renderer $= V4 0 0 0 255
      clear renderer
      present renderer
      unless (qPressed events) (appLoop renderer)
      where
        eventIsQPress event =
          case eventPayload event of
            KeyboardEvent keyboardEvent ->
              keyboardEventKeyMotion keyboardEvent == Pressed
                && keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
            _ -> False
        qPressed = any eventIsQPress