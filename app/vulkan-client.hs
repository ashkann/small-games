{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Exception
import Control.Monad ((>=>))
import Control.Monad.IO.Class
import Control.Monad.Managed
import Data.Foldable (traverse_)
import DearImGui
import DearImGui.SDL
import Foreign.C.String (peekCString)
import qualified SDL

main :: IO ()
main = do
  SDL.initializeAll

  -- Create a window using SDL.
  let title = "Hello, Dear ImGui!"
  let config = SDL.defaultWindow {SDL.windowGraphicsContext = SDL.VulkanContext}
  window <- SDL.createWindow title config

  extensions <- Vlk.vkGetInstanceExtensions window
  traverse_ (peekCString >=> putStrLn) extensions

  -- Create an OpenGL context
  -- glContext <- managed $ bracket (glCreateContext window) glDeleteContext

  -- Create an ImGui context
  imgui <- createContext
  -- Initialize ImGui's SDL2 backend
  -- _ <- managed_ $ bracket_ (sdl2InitForOpenGL window glContext) sdl2Shutdown

  -- Initialize ImGui's OpenGL backend
  -- _ <- managed_ $ bracket_ openGL3Init openGL3Shutdown

  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  info <- SDL.getRendererInfo renderer
  print info

  liftIO $ mainLoop window

  destroyContext imgui
  SDL.destroyRenderer renderer
  SDL.destroyWindow window

-- mainLoop :: Window -> IO ()
mainLoop window = unlessQuit do
  -- Tell ImGui we're starting a new frame

  --   rendererDrawColor renderer SDL.$= SDL.V4 0 255 0 128
  --   clear renderer
  --   rendererDrawColor renderer SDL.$= SDL.V4 0 0 255 128
  --   drawLine renderer (SDL.P (SDL.V2 10 10)) (SDL.P (SDL.V2 100 100))
  --   putStrLn "Frame ***************"
  -- drawPointF renderer (SDL.P (SDL.V2 10 10)) -- (SDL.P (SDL.V2 100 100))

  -- openGL3NewFrame
  -- sdl2NewFrame
  -- newFrame
  -- -- Show the ImGui demo window
  -- showDemoWindow

  -- endFrame
  -- sdl2EndFrame
  -- openGL3E
  -- render
  -- openGL3RenderDrawData =<< getDrawData

  SDL.glSwapWindow window

  mainLoop window
  where
    -- Process the event loop
    unlessQuit action = do
      shouldQuit <- checkEvents
      if shouldQuit then pure () else action

    checkEvents = do
      pollEventWithImGui >>= \case
        Nothing ->
          return False
        Just event ->
          (isQuit event ||) <$> checkEvents

    isQuit event =
      SDL.eventPayload event == SDL.QuitEvent