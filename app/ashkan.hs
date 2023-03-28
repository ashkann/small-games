{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- import Controlx.Exception
-- import Control.Monad.Managed

-- import Graphics.GL

import Control.Monad (unless)
-- import Control.Monad.IO.Class
import DearImGui qualified as ImGui
import DearImGui.OpenGL3 qualified as ImGuiRenderer
import DearImGui.SDL qualified as ImGuiPlatform2
import DearImGui.SDL.OpenGL qualified as ImGuiPlatform
import SDL qualified
import qualified Data.StateVar

main :: IO ()
main = do
  SDL.initializeAll

  putStrLn "Create main window"
  let gl3 = SDL.Core SDL.Debug 2 1
  let config = SDL.defaultWindow {SDL.windowGraphicsContext = SDL.OpenGLContext SDL.defaultOpenGL {SDL.glProfile = gl3}}
  window <- SDL.createWindow "Tic-Tac-Toe" config

  putStrLn "Create GL context"
  glContext <- SDL.glCreateContext window

  putStrLn "Created ImGui context"
  context <- ImGui.createContext

  putStrLn "Init ImGui platform"
  _ <- ImGuiPlatform.sdl2InitForOpenGL window glContext
--   f <- ImGuiPlatform.sdl2InitForOpenGL window glContext
--   print f  

  putStrLn "Init ImGui renderer"
  _ <- ImGuiRenderer.openGL3Init
--   f2 <- ImGuiRenderer.openGL3Init
--   print f2

  putStrLn "Create SDL renderer"
  SDL.HintRenderDriver Data.StateVar.$= SDL.OpenGL
--   print =<< Data.StateVar.get SDL.HintRenderDriver
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

  appLoop window renderer

  ImGuiPlatform2.sdl2Shutdown
  ImGui.destroyContext context
  SDL.destroyWindow window
  where
    appLoop window renderer = do
      events <- SDL.pollEvents

      ImGuiRenderer.openGL3NewFrame
      ImGuiPlatform2.sdl2NewFrame
      ImGui.newFrame

    --   ImGui.showDemoWindow

      SDL.rendererDrawColor renderer SDL.$= SDL.V4 0 255 0 128
      SDL.clear renderer
      SDL.rendererDrawColor renderer SDL.$= SDL.V4 0 0 255 128
      SDL.drawLine renderer (SDL.P (SDL.V2 0 0)) (SDL.P (SDL.V2 1 1))

      -- Build the GUI
      --   ImGuiRenderer.withWindowOpen "Hello, ImGui!" $ do
      --     ImGuiRenderer.text "Hello, ImGui!"
      --     ImGuiRenderer.button "Clickety Click" >>= \case
      --       False -> return ()
      --       True -> putStrLn "Ow!"

      --   _ <- glClear GL_COLOR_BUFFER_BIT
      --   SDL.glView
      ImGuiRenderer.openGL3RenderDrawData =<< ImGui.getDrawData
      -- End GUI

      ImGui.render

      SDL.glSwapWindow window

      unless (qPressed events) (appLoop window renderer)
      where
        eventIsQPress event =
          case SDL.eventPayload event of
            SDL.KeyboardEvent keyboardEvent ->
              SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed
                && SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeQ
            _ -> False
        qPressed = any eventIsQPress