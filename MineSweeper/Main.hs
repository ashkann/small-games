{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module Main (main) where

import Data.Array.IArray (Array, Ix (..), array, assocs, (!), (//))
import Graphics.Gloss.Interface.Pure.Game (Color, Display (InWindow), Event (EventKey), Key (MouseButton), KeyState (Down, Up), Modifiers (Modifiers), MouseButton (LeftButton, RightButton), Picture (Text), black, blank, blue, circle, color, cyan, green, greyN, line, makeColor, orange, pictures, play, rectangleSolid, rectangleWire, red, scale, translate, white, yellow)
import Prelude hiding (Left, Right)

data Position = Position Float Float deriving (Eq, Ord)

data Count = Zero | One | Two | Three | Four | Five | Six | Seven | Eight deriving (Eq, Enum)

data Cell = Flaged | Opened Count | UnOpened

data XY = XY Int Int deriving (Eq, Ord, Ix)

newtype Board = Board (Array XY Cell)

data World = World {board :: Board, mines :: [XY]}

world0 :: World
world0 = World (Board clean) mines
  where
    bounds = let lower = XY 0 0; upper = XY (boardWidth - 1) (boardHeight - 1) in (lower, upper)
    clean = array bounds $ map (,UnOpened) (range bounds)
    mines = [XY 0 0, XY 0 1, XY 2 3, XY 3 3, XY 3 4, XY 4 4, XY 4 5, XY 5 5, XY 5 6]

drawWorld :: World -> Picture
drawWorld (World field _) = pictures [drawBoard field]

drawCords :: Picture
drawCords = color blue $ pictures [line [(0, 0), (0, fromIntegral windowHeight)], line [(0, 0), (fromIntegral windowWidth, 0)]]

size :: (Real a) => a -> Float
size x = realToFrac x * blockSize

halfSize :: (Real a) => a -> Float
halfSize x = size x / 2

blocks :: Float -> Float
blocks x = x / blockSize

fromField :: XY -> Position
fromField (XY x y) = Position (size x) (size y)

toXY :: Position -> XY
toXY (Position x y) = XY (floor . blocks $ x) (floor . blocks $ y)

boardWidth :: Int
boardWidth = 6

boardHeight :: Int
boardHeight = 7

drawBoard :: Board -> Picture
drawBoard (Board field) = center $ pictures $ map (\(xy, cell) -> at xy $ drawCell cell) (assocs field)
  where
    at (XY x y) = translate (size x) (size y)
    center = translate (negate . halfSize $ boardWidth - 1) (negate . halfSize $ boardHeight - 1)

drawCell :: Cell -> Picture
drawCell Flaged = pictures [square, flag] where flag = color red $ rectangleSolid 10 10
drawCell (Opened c) = pictures [square, color (for c) (translate (-blockSize / 2 + 5) (-blockSize / 2 + 5) $ scale 0.2 0.2 $ pictures [circle 2.0, number c])]
drawCell UnOpened = square

number :: Count -> Picture
number Zero = pictures [Text "0"]
number One = pictures [Text "1"]
number Two = pictures [Text "2"]
number Three = pictures [Text "3"]
number Four = pictures [Text "4"]
number Five = pictures [Text "5"]
number Six = pictures [Text "6"]
number Seven = pictures [Text "7"]
number Eight = pictures [Text "8"]

for :: Count -> Color
for Zero = white
for One = red
for Two = orange
for Three = yellow
for Four = makeColor 0.6 0.8 0.2 1.0
for Five = green
for Six = makeColor 0 0.6 0.4 1.0
for Seven = cyan
for Eight = blue

square :: Picture
square = pictures [cell, border]
  where
    cell = color (greyN 0.7) $ rectangleSolid blockSize blockSize
    border = color white $ rectangleWire blockSize blockSize

-- drawCell Hidden = color blue $ rectangleSolid 10 10

drawMouse :: Position -> Picture
drawMouse (Position x y) = blank -- translate x y $ color white $ rectangleSolid 10 10

update :: Float -> World -> World
update _ w = w

data Adjacent = A0 | A1 | A2 | A3 | A4 | A5 | A6 | A7 deriving (Eq, Enum)

adjacents :: Adjacent -> Adjacent -> XY -> [XY]
adjacents from to xy = adj from xy : if from == to then [] else adjacents (next from) to xy
  where
    next A7 = A0
    next a = succ a
    adj a (XY x y) =
      let (dx, dy) =
            ( case a of
                A0 -> (-1, -1)
                A1 -> (-1, 0)
                A2 -> (-1, 1)
                A3 -> (0, 1)
                A4 -> (1, 1)
                A5 -> (1, 0)
                A6 -> (1, -1)
                A7 -> (0, -1)
            )
       in XY (x + dx) (y + dy)

open :: XY -> [XY] -> Board -> Board
open xy mines (Board field) = Board $ field // go xy
  where
    go xy = (xy, Opened $ count xy) : (if count xy == Zero then surroundings xy else [])
      where
        surroundings xy = foldl (\acc xy -> go xy ++ acc) [] (adj xy)
        count xy = foldl (\c xy -> if xy `elem` mines then succ c else c) Zero (adj xy)
        adj xy@(XY x y) = adjacents from to xy
          where
            (from, to)
              | bottom && left = (A3, A5)
              | top && left = (A5, A7)
              | top && right = (A7, A1)
              | bottom && right = (A1, A3)
              | left = (A3, A7)
              | right = (A7, A3)
              | bottom = (A1, A5)
              | top = (A5, A1)
              | otherwise = (A0, A7)
            bottom = y == 0
            top = y == boardHeight - 1
            left = x == 0
            right = x == boardWidth - 1

event :: Event -> World -> World
event (EventKey (MouseButton LeftButton) Down (Modifiers Up Up Up) (x, y)) w@(World board mines) =
  if xy `elem` mines then world0 else w {board = open xy mines board}
  where
    xy = toXY pos
    pos = Position (halfSize boardWidth + x) (halfSize boardHeight + y)
event (EventKey (MouseButton RightButton) Down (Modifiers Up Up Up) (x, y)) w = flag (toXY pos) w
  where
    pos = Position (halfSize boardWidth + x) (halfSize boardHeight + y)
    flag (XY x y) w@(World (Board board) _) = w {board = Board board'}
      where
        old = board ! XY x y
        board' = case old of
          Flaged -> board // [(XY x y, UnOpened)]
          UnOpened -> board // [(XY x y, Flaged)]
          _ -> board
event _ w = w

-- event e w@(World filed position)
-- --   | Just k <- special e = case k of
-- --       KeyUp -> w
-- --       KeyDown -> w
-- --       KeyLeft -> w
-- --       KeyRight -> w
--     --   _ -> w
--    = w
--   where
--     special (EventKey (SpecialKey k) G.Down _ _) = Just k
--     special _ = Nothing

windowWidth :: Int
windowWidth = 500

windowHeight :: Int
windowHeight = 500

blockSize :: Float
blockSize = 30

main :: IO ()
main =
  play
    (InWindow "Mine Sweeper" (windowWidth, windowHeight) (10, 10))
    black
    5
    world0
    drawWorld
    event
    update