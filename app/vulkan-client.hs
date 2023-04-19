{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Exception (bracket, bracket_)
import Control.Monad (unless)
import Control.Monad.IO.Class
import Control.Monad.Managed
import Data.Bits
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Vector qualified as V
import DearImGui.SDL
import SDL qualified
import SDL.Video.Vulkan qualified as SDL
import Vulkan.CStruct.Extends
import Vulkan.Core10
import Vulkan.Core13
import Vulkan.Extensions
import Vulkan.Utils.Debug
import Vulkan.Zero

-- import Vulkan.Core10.DeviceInitialization qualified as V1.DeviceInitialization
-- import Vulkan.Core13 qualified as V3

main :: IO ()
main = runManaged $ do
  withSDL
  window <- createWindow
  exts <- getWindowExts window
  liftIO $ print exts
  let debugUtilsMessengerCreateInfo =
        zero
          { messageSeverity =
              DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT
                .|. DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT,
            messageType =
              DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT
                .|. DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT
                .|. DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT,
            pfnUserCallback = debugCallbackPtr
          }
  let createInstanceInfo =
        zero
          { applicationInfo = Just zero {applicationName = Just "vulkan-client", apiVersion = API_VERSION_1_0},
            enabledLayerNames = V.fromList $ [BS.empty],
            enabledExtensionNames = V.fromList [BS.empty]
          }
          ::& debugUtilsMessengerCreateInfo
          :& ValidationFeaturesEXT [VALIDATION_FEATURE_ENABLE_BEST_PRACTICES_EXT] []
          :& ()
  vlk <- withInstance createInstanceInfo Nothing allocate
  return ()
  where
    allocate :: IO a -> (a -> IO ()) -> Managed a
    allocate c d = managed (bracket c d)

    withSDL = managed_ $ bracket_ (SDL.initializeAll) SDL.quit
    getWindowExts w = liftIO $ traverse BS.packCString =<< SDL.vkGetInstanceExtensions w
    createWindow = managed $ bracket (SDL.createWindow title config) SDL.destroyWindow
      where
        title = "Vulkan + Dear ImGui + SDL (Input, Window, Audio)"
        config = SDL.defaultWindow {SDL.windowGraphicsContext = SDL.VulkanContext}

-- extensions <- liftIO $ Vlk.vkGetInstanceExtensions window
-- traverse_ (peekCString >=> putStrLn) extensions

-- Create an ImGui context
--   imgui <- createContext
-- Initialize ImGui's SDL2 backend
-- _ <- managed_ $ bracket_ (sdl2InitForOpenGL window glContext) sdl2Shutdown

-- Initialize ImGui's OpenGL backend
-- _ <- managed_ $ bracket_ openGL3Init openGL3Shutdown
--   info <- SDL.getRendererInfo renderer
--   print info

--   destroyContext imgui
--

-- mainLoop :: Window -> IO ()
-- mainLoop2 window = do
--   events <- SDL.pollEvents
--   SDL.rendererDrawColor renderer SDL.$= SDL.V4 0 255 0 255
--   SDL.clear renderer
--   SDL.present renderer
--   unless (qPressed events) (mainLoop2 renderer window)
--   where
--     eventIsQPress event =
--       case SDL.eventPayload event of
--         SDL.KeyboardEvent keyboardEvent -> SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed && SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeQ
--         _ -> False
--     qPressed = any eventIsQPress

-- mainLoop window = unlessQuit do
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

-- mainLoop window
-- where
--   -- Process the event loop
--   unlessQuit action = do
--     shouldQuit <- checkEvents
--     unless shouldQuit action

--   checkEvents = do
--     pollEventWithImGui >>= \case
--       Nothing ->
--         return False
--       Just event ->
--         (isQuit event ||) <$> checkEvents

--   isQuit event =
--     SDL.eventPayload event == SDL.QuitEvent