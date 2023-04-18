{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Exception (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class
import Control.Monad.Managed
import DearImGui.SDL
import SDL qualified

main :: IO ()
main = do
  SDL.initializeAll
  print =<< SDL.getRenderDriverInfo

  -- Create a window using SDL.
  let title = "SDL Vulkan + Dear ImGui"
  let config = SDL.defaultWindow {SDL.windowGraphicsContext = SDL.VulkanContext}

  runManaged $ do
    window <- managed $ bracket (SDL.createWindow title config) SDL.destroyWindow
    -- extensions <- liftIO $ Vlk.vkGetInstanceExtensions window
    -- traverse_ (peekCString >=> putStrLn) extensions

    -- Create an ImGui context
    --   imgui <- createContext
    -- Initialize ImGui's SDL2 backend
    -- _ <- managed_ $ bracket_ (sdl2InitForOpenGL window glContext) sdl2Shutdown

    -- Initialize ImGui's OpenGL backend
    -- _ <- managed_ $ bracket_ openGL3Init openGL3Shutdown
    SDL.HintRenderDriver SDL.$= SDL.OpenGL
    renderer <- managed $ bracket (SDL.createRenderer window (-1) SDL.defaultRenderer) SDL.destroyRenderer
    --   info <- SDL.getRendererInfo renderer
    --   print info

    liftIO $ mainLoop2 renderer window

--   destroyContext imgui
--

-- mainLoop :: Window -> IO ()
mainLoop2 renderer window = do
  events <- SDL.pollEvents
  SDL.rendererDrawColor renderer SDL.$= SDL.V4 0 255 0 255
  SDL.clear renderer
  SDL.present renderer
  unless (qPressed events) (mainLoop2 renderer window)
  where
    eventIsQPress event =
      case SDL.eventPayload event of
        SDL.KeyboardEvent keyboardEvent -> SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed && SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeQ
        _ -> False
    qPressed = any eventIsQPress

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

  --   SDL.glSwapWindow window

  mainLoop window
  where
    -- Process the event loop
    unlessQuit action = do
      shouldQuit <- checkEvents
      unless shouldQuit action

    checkEvents = do
      pollEventWithImGui >>= \case
        Nothing ->
          return False
        Just event ->
          (isQuit event ||) <$> checkEvents

    isQuit event =
      SDL.eventPayload event == SDL.QuitEvent