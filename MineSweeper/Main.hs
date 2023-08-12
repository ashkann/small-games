{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module Main (main) where

import Data.Array.IArray (Array, Ix (..), array, assocs, (!), (//))
import Debug.Trace (trace)
import Graphics.Gloss.Interface.Pure.Game (Color, Display (InWindow), Event (EventKey), Key (MouseButton), KeyState (Down, Up), Modifiers (Modifiers), MouseButton (LeftButton, RightButton), Picture (Text), black, blank, blue, circle, color, cyan, green, greyN, line, makeColor, orange, pictures, play, rectangleSolid, rectangleWire, red, scale, translate, white, yellow)
import Prelude hiding (Left, Right)

data ScreenPos = ScreenPos Float Float deriving (Eq, Ord)

data Count = Zero | One | Two | Three | Four | Five | Six | Seven | Eight deriving (Eq, Enum)

data Cell = Flaged | Opened Count | UnOpened

data CellPos = CellPos Int Int deriving (Eq, Ord, Ix, Show)

newtype Board = Board (Array CellPos Cell)

data World = World {board :: Board, mines :: [CellPos]}

world0 :: World
world0 = World (Board clean) mines
  where
    bounds = let lower = CellPos 0 0; upper = CellPos (boardWidth - 1) (boardHeight - 1) in (lower, upper)
    clean = array bounds $ map (,UnOpened) (range bounds)
    mines = [CellPos 0 0, CellPos 0 1, CellPos 2 3, CellPos 3 3, CellPos 3 4, CellPos 4 4, CellPos 4 5, CellPos 5 5, CellPos 5 6]

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

screen2cell :: Float -> Float -> CellPos
screen2cell x y =
  let boardX = x + halfSize boardWidth
      boardY = y + halfSize boardHeight
      cellX = floor $ blocks boardX
      cellY = floor $ blocks boardY
   in trace (show cellX ++ ", " ++ show cellY) (CellPos cellX cellY)

boardWidth :: Int
boardWidth = 6

boardHeight :: Int
boardHeight = 7

drawBoard :: Board -> Picture
drawBoard (Board field) = center $ pictures $ map (\(xy, cell) -> at xy $ drawCell cell) (assocs field)
  where
    at (CellPos x y) = translate (size x) (size y)
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

-- drawCell Hidden = color blue $ rectangleSolid 10 10 translate x y $ color white $ rectangleSolid 10 10

update :: Float -> World -> World
update _ w = w

open :: CellPos -> [CellPos] -> Board -> Board
open p mines (Board b) = Board $ b // go [] p
  where
    go stop p =
      if p `elem` stop
        then []
        else
          let ps = neighbors p
              !_ = trace ("open " ++ show p) ()
              count = foldl (\c p -> if p `elem` mines then succ c else c) Zero ps
           in (p, Opened count) : (if count == Zero then concatMap (go $ p : stop) ps else [])

flag :: CellPos -> Board -> Board
flag pos (Board b) = Board $ b // change
  where
    change =
      case b ! pos of
        Flaged -> [(pos, UnOpened)]
        UnOpened -> [(pos, Flaged)]
        _ -> []

neighbors :: CellPos -> [CellPos]
neighbors (CellPos x y) =
  foldl
    ( \ps (dx, dy) ->
        let x' = x + dx
            y' = y + dy
         in if (x' >= 0 && x' < boardWidth) && (y' >= 0 && y' < boardHeight)
              then CellPos (x + dx) (y + dy) : ps
              else ps
    )
    []
    ds
  where
    ds =
      [ (-1, -1),
        (-1, 0),
        (-1, 1),
        (0, 1),
        (1, 1),
        (1, 0),
        (1, -1),
        (0, -1)
      ]

event :: Event -> World -> World
event e w
  | Just (x, y) <- leftClicked,
    Just pos <- insideBoard x y =
      let World board mines = w
          lost = world0
       in if pos `elem` mines then lost else w {board = open pos mines board}
  | Just (x, y) <- rightClicked,
    Just pos <- insideBoard x y =
      let World board _ = w in w {board = flag pos board}
  | otherwise = w
  where
    clicked
      | EventKey (MouseButton btn) Down (Modifiers Up Up Up) (x, y) <- e = Just (btn, (x, y))
      | otherwise = Nothing
    leftClicked
      | Just (LeftButton, xy) <- clicked = Just xy
      | otherwise = Nothing
    rightClicked
      | Just (RightButton, xy) <- clicked = Just xy
      | otherwise = Nothing
    insideBoard x y =
      let left = negate $ halfSize boardWidth
          right = halfSize boardWidth
          bottom = negate $ halfSize boardHeight
          top = halfSize boardHeight
       in if left <= x && x <= right && bottom <= y && y <= top then Just $ screen2cell x y else Nothing

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