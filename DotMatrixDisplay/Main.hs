{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BangPatterns #-}


module Main (main) where

import BitmapFont
import Control.Monad.State.Lazy
import Graphics.Gloss.Interface.Pure.Game
  ( Display (InWindow),
    Picture,
    black,
    blue,
    color,
    green,
    pictures,
    play,
    translate,
    yellow,
  )
import Graphics.Gloss.Interface.Pure.Game qualified as G
import Debug.Trace (trace, traceShow)
import Data.Map.Strict qualified as M


windowWidth :: Int
windowWidth = 1000

windowHeight :: Int
windowHeight = 500

-- data KeyboardEvent = Typed Char | MoveLeft | MoveRight | DeleteLeft

-- updateInput :: Input -> KeyboardEvent -> Input
-- updateInput i@(Input s _) ev = case ev of
--   Typed ch -> i {s = s ++ [ch]}
--   MoveLeft -> i
--   MoveRight -> i
--   DeleteLeft -> if null s then i else i {s = init s}

-- data Input = Input {s :: String}

data World = World {font :: Font, time :: Float}

tick :: Float -> World -> World
tick dt w@(World {time = t}) = w {time = t + dt}

event :: G.Event -> World -> World
event _ w = w
-- event e w@(World font t i)
--   | Just ev <- keyboard = w {input = updateInput i ev}
--   | otherwise = w
--   where
--     keyboard
--       | G.EventKey (G.Char ch) G.Down (G.Modifiers _ G.Up G.Up) _ <- e = Just $ Typed ch
--       | G.EventKey (G.SpecialKey G.KeyDelete) G.Down (G.Modifiers G.Up G.Up G.Up) _ <- e = Just DeleteLeft
--       | otherwise = Nothing

draw :: World -> Picture
draw (World font t) =
  let l1 = "Seconds passed: " ++ show (floor t :: Int)
      l2 = "ABCDEFGHIJKLMNOP±QRSTUVWXYZ abcdefghijklmnopqrstuvwxyz 1234567890 !@#$%^&*()_+ ±"
      s1 = sqaureDotStyle G.white 1 0
      s2 = sqaureDotStyle blue 2 0
      s3 = sqaureDotStyle green 3 0
      s4 = sqaureDotStyle yellow 4 0
      lines = [(l2, s1), (l2, s2), (l2, s3), (l1, s4)]
      x = negate . fromIntegral $ (windowWidth `div` 2) - 10
      layout (str, style) = do
        y <- get
        _ <- put $ y + height style
        let coord = color green $ pictures [G.line [(0, 0), (0, 10)], G.line [(0, 0), (10, 0)]]
        return $ translate x y (pictures $ coord : [drawText style font str])
   in pictures $ evalState (mapM layout lines) 0

game :: Font -> IO ()
game font =
  play
    (InWindow "Dot Matrix Display" (windowWidth, windowHeight) (10, 10))
    black
    20
    (World font 0.0)
    draw
    event
    tick

main :: IO ()
main = do
  font <- readFont "unifont_all-15.1.02.hex"
  -- let !_ = traceShow (M.lookup 'A' font ) ()
  game font
