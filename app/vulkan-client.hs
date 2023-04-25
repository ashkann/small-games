{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Applicative ((<|>))
import Control.Exception (bracket, bracket_)
import Control.Monad.Except
import Control.Monad.Managed
import Control.Monad.Trans.Maybe
import Data.Bits
import Data.ByteString.Char8 qualified as BS8
import Data.List (nub)
import Data.Vector qualified as V
import Data.Word
import Foreign.Ptr (castPtr)
import SDL qualified
import SDL.Video.Vulkan qualified as SDL
import Vulkan.CStruct.Extends
import Vulkan.Core10
-- import Vulkan.Core12
-- import Vulkan.Core13
import Vulkan.Extensions
import Vulkan.Utils.Debug
import Vulkan.Zero

-- import Vulkan.Dynamic (InstanceCmds(pVkEnumeratePhysicalDevices))

-- import Vulkan.Core10.DeviceInitialization qualified as V1.DeviceInitialization
-- import Vulkan.Core13 qualified as V3

type VulkanMonad = ExceptT String Managed

main :: IO ()
main = runManaged $ do
  result <- runExceptT program
  case result of
    Right _ -> pure ()
    Left e -> liftIO $ putStrLn e
  where
    program :: VulkanMonad ()
    program = do
      withSDL
      window <- createWindow
      --   -- exts <- getWindowExts window
      --   -- liftIO $ print exts
      let createInstanceInfo =
            zero
              { applicationInfo = Just zero {applicationName = Just "vulkan-client", apiVersion = API_VERSION_1_0},
                enabledLayerNames = V.fromList ["VK_LAYER_KHRONOS_validation"],
                enabledExtensionNames =
                  V.fromList
                    [ EXT_DEBUG_UTILS_EXTENSION_NAME,
                      EXT_VALIDATION_FEATURES_EXTENSION_NAME,
                      "VK_KHR_surface",
                      "VK_MVK_macos_surface",
                      "VK_EXT_metal_surface"
                    ]
              }
              ::& debugUtilsMessengerCreateInfo
              :& ValidationFeaturesEXT [VALIDATION_FEATURE_ENABLE_BEST_PRACTICES_EXT] []
              :& ()

      inst <- withInstance createInstanceInfo Nothing allocate
      surface <- withSurface inst window
      -- _ <- withDebugUtilsMessengerEXT inst debugUtilsMessengerCreateInfo Nothing allocate
      --   -- submitDebugUtilsMessageEXT
      --   --   inst
      --   --   DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT
      --   --   DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT
      --   --   zero {message = "Debug Message Test"}
      (_, gpus) <- enumeratePhysicalDevices inst
      (gpu, gfx, present) <- pickGpu gpus surface `orErr` "Can't find a matching GPU"
      liftIO . putStrLn . ("Picked GPU: " ++) . BS8.unpack . deviceName =<< getPhysicalDeviceProperties gpu
      let deviceCreateInfo =
            zero
              { queueCreateInfos = V.fromList qs,
                enabledExtensionNames = [KHR_SWAPCHAIN_EXTENSION_NAME]
              }
            where
              qs = [SomeStruct $ zero {queueFamilyIndex = i, queuePriorities = [1]} | i <- nub [gfx, present]]
      dev <- withDevice gpu deviceCreateInfo Nothing allocate
      gfxQ <- getDeviceQueue dev gfx 0
      presentQ <- getDeviceQueue dev present 0
      surfaceCaps <- getPhysicalDeviceSurfaceCapabilitiesKHR gpu surface
      (_, formats) <- getPhysicalDeviceSurfaceFormatsKHR gpu surface
      let swapchainCreateInfo =
            let (mode, qs) = if gfxQ == presentQ then (SHARING_MODE_EXCLUSIVE, []) else (SHARING_MODE_CONCURRENT, [gfx, present])
                extent = case currentExtent (surfaceCaps :: SurfaceCapabilitiesKHR) of
                  Extent2D w h | w == maxBound, h == maxBound -> Extent2D (fromIntegral width) (fromIntegral height)
                  e -> e
                first = V.head formats
                c = colorSpace (first :: SurfaceFormatKHR)
                f = format (first :: SurfaceFormatKHR)
             in zero
                  { surface = surface,
                    minImageCount = minImageCount (surfaceCaps :: SurfaceCapabilitiesKHR) + 1,
                    -- imageFormat = (format :: SurfaceFormatKHR -> Format) surfaceFormat,
                    imageFormat = f, -- FORMAT_B8G8R8_UNORM,
                    imageColorSpace = c, -- COLOR_SPACE_SRGB_NONLINEAR_KHR,
                    imageExtent = extent,
                    imageArrayLayers = 1,
                    imageUsage = IMAGE_USAGE_COLOR_ATTACHMENT_BIT,
                    imageSharingMode = mode,
                    queueFamilyIndices = qs,
                    preTransform = currentTransform (surfaceCaps :: SurfaceCapabilitiesKHR),
                    compositeAlpha = COMPOSITE_ALPHA_OPAQUE_BIT_KHR,
                    presentMode = PRESENT_MODE_FIFO_KHR,
                    clipped = True
                  }
      swapChain <- withSwapchainKHR dev swapchainCreateInfo Nothing allocate
      return ()
      where
        f `orErr` e = maybeToExceptT e f
        -- allocate :: IO a -> (a -> IO ()) -> Managed a
        allocate c d = managed (bracket c d)
        width = 300 :: Int
        height = 300 :: Int

-- -- | Get the Vulkan surface for an SDL window
-- getSDLWindowSurface :: Instance -> SDL.Window -> IO SurfaceKHR
-- getSDLWindowSurface inst window =
--   SurfaceKHR <$> SDL.vkCreateSurface window (castPtr (instanceHandle inst))

withSurface :: Instance -> SDL.Window -> VulkanMonad SurfaceKHR
withSurface inst window = managed $ bracket create destroy
  where
    vkInst = castPtr (instanceHandle inst)
    create = SurfaceKHR <$> SDL.vkCreateSurface window vkInst
    destroy s = destroySurfaceKHR inst s Nothing

pickGpu :: MonadIO m => V.Vector PhysicalDevice -> SurfaceKHR -> MaybeT m (PhysicalDevice, Word32, Word32)
pickGpu gpus surface = case V.uncons gpus of
  Just (g, gs) ->
    let thisOne =
          ( do
              qs <- getPhysicalDeviceQueueFamilyProperties g
              i1 <- MaybeT . pure $ gfx qs
              i2 <- prs g (V.length qs) 0
              return (g, fromIntegral i1, fromIntegral i2)
          )
     in thisOne <|> pickGpu gs surface
  Nothing -> MaybeT $ pure Nothing
  where
    gfx qs =
      let check q = (QUEUE_GRAPHICS_BIT .&. queueFlags q /= zeroBits) && (queueCount q > 0)
       in fst <$> V.find (check . snd) (V.indexed qs)
    prs gpu len i =
      if i < len
        then do
          check <- getPhysicalDeviceSurfaceSupportKHR gpu (fromIntegral i) surface
          let thisOne = if check then pure i else MaybeT $ pure Nothing
          thisOne <|> prs gpu len (i + 1)
        else MaybeT $ pure Nothing

withSDL :: VulkanMonad ()
withSDL = managed_ $ bracket_ (SDL.initialize ([SDL.InitVideo] :: [SDL.InitFlag])) SDL.quit

createWindow = managed $ bracket (SDL.createWindow title config) SDL.destroyWindow
  where
    title = "Vulkan + Dear ImGui + SDL (Input, Window, Audio)"
    config = SDL.defaultWindow {SDL.windowGraphicsContext = SDL.VulkanContext}

debugUtilsMessengerCreateInfo =
  zero
    { messageSeverity =
        DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT
          .|. DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT,
      -- .|. DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT,
      messageType =
        DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT
          .|. DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT
          .|. DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT,
      pfnUserCallback = debugCallbackPtr
    }

-- deviceCreateInfo i =
--   zero
--     { queueCreateInfos = V.fromList [SomeStruct $ zero {queueFamilyIndex = i, queuePriorities = [1]}],
--       enabledExtensionNames = [KHR_SWAPCHAIN_EXTENSION_NAME]
--     }

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