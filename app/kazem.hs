{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Managed
import DearImGui
import DearImGui.OpenGL3
import DearImGui.SDL
import DearImGui.SDL.OpenGL
import SDL
import qualified SDL.Video.Renderer as X

main :: IO ()
main = do
  -- Initialize SDL
  initializeAll
  HintRenderDriver $= OpenGL

  runManaged do
    -- Create a window using SDL. As we're using OpenGL, we need to enable OpenGL too.
    window <- do
      let title = "Hello, Dear ImGui!"
      let gl3 = SDL.Core SDL.Debug 2 1
      -- let config = defaultWindow {windowGraphicsContext = OpenGLContext defaultOpenGL}
      let config = SDL.defaultWindow {SDL.windowGraphicsContext = SDL.OpenGLContext SDL.defaultOpenGL {SDL.glProfile = gl3}}
      managed $ bracket (createWindow title config) destroyWindow

    -- Create an OpenGL context
    glContext <- managed $ bracket (glCreateContext window) glDeleteContext

    -- Create an ImGui context
    _ <- managed $ bracket createContext destroyContext

    -- Initialize ImGui's SDL2 backend
    _ <- managed_ $ bracket_ (sdl2InitForOpenGL window glContext) sdl2Shutdown

    -- Initialize ImGui's OpenGL backend
    _ <- managed_ $ bracket_ openGL3Init openGL3Shutdown

    SDL.HintRenderDriver SDL.$= SDL.OpenGL
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer    

    liftIO $ mainLoop window renderer

-- mainLoop :: Window -> IO ()
mainLoop window renderer = unlessQuit do
  -- Tell ImGui we're starting a new frame
  
  rendererDrawColor renderer SDL.$= SDL.V4 0 255 0 128
  clear renderer
  rendererDrawColor renderer SDL.$= SDL.V4 0 0 255 128
  drawLine renderer (SDL.P (SDL.V2 10 10)) (SDL.P (SDL.V2 100 100))
  putStrLn "Frame ***************"
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

  mainLoop window renderer
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