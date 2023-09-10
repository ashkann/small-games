{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module Main (main) where

import Control.Monad.IO.Class (MonadIO)
import Data.Array.IArray (Array, Ix (..), array, assocs, (!), (//))
import Debug.Trace (trace)
import Graphics.Gloss.Interface.IO.Game
  ( Color,
    Display (InWindow),
    Event (EventKey),
    Key (MouseButton),
    KeyState (Down, Up),
    Modifiers (Modifiers),
    MouseButton (LeftButton, RightButton),
    Picture (Text),
    black,
    blue,
    circle,
    color,
    cyan,
    green,
    greyN,
    makeColor,
    orange,
    pictures,
    playIO,
    rectangleSolid,
    rectangleWire,
    red,
    scale,
    translate,
    white,
    yellow,
  )
import Graphics.UI.GLUT.Fonts qualified as GLUT
import Prelude hiding (Left, Right)

data ScreenPos = ScreenPos Float Float deriving (Eq, Ord)

data Count = Zero | One | Two | Three | Four | Five | Six | Seven | Eight deriving (Eq, Enum)

data Cell = Flaged | Opened Count | UnOpened deriving (Eq)

data CellPos = CellPos Int Int deriving (Eq, Ord, Ix, Show)

newtype Board = Board (Array CellPos Cell)

data World = World {board :: Board, mines :: [CellPos]}

world0 :: World
world0 = World (Board clean) mines
  where
    bounds = let lower = CellPos 0 0; upper = CellPos (boardWidth - 1) (boardHeight - 1) in (lower, upper)
    clean = array bounds $ map (,UnOpened) (range bounds)
    mines = [CellPos 0 0, CellPos 0 1, CellPos 2 3, CellPos 3 3, CellPos 3 4, CellPos 4 4, CellPos 4 5, CellPos 5 5, CellPos 5 6]

drawWorld :: MonadIO m => World -> m Picture
drawWorld (World field _) = do restart <- drawRestart; return $ pictures [drawBoard field, restart]

size :: (Real a) => a -> Float
size x = realToFrac x * cellSize

halfSize :: (Real a) => a -> Float
halfSize x = size x / 2

blocks :: Float -> Float
blocks x = x / cellSize

screen2cell :: Float -> Float -> CellPos
screen2cell x y =
  let boardX = x + halfSize boardWidth
      boardY = y + halfSize boardHeight
      cellX = floor $ blocks boardX
      cellY = floor $ blocks boardY
   in CellPos cellX cellY

drawCell :: Cell -> Picture
drawCell Flaged = pictures [square, flag] where flag = color red $ rectangleSolid 10 10
drawCell (Opened c) = pictures [square, color (for c) (translate (-cellSize / 2 + 5) (-cellSize / 2 + 5) $ scale 0.2 0.2 $ pictures [circle 2.0, number c])]
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
    cell = color (greyN 0.7) $ rectangleSolid cellSize cellSize
    border = color white $ rectangleWire cellSize cellSize

open :: CellPos -> World -> World
open p0 w@(World (Board b0) mines) = w {board = Board $ go b0 p0}
  where
    go b p
      | b ! p /= UnOpened = b
      | otherwise = do
          let ps = neighbors p
              -- !_ = trace ("open " ++ show p) ()
              count = foldl (\c p -> if p `elem` mines then succ c else c) Zero ps
              b' = b // [(p, Opened count)]
           in if count /= Zero then b' else foldl go b' ps

isMine :: CellPos -> World -> Bool
isMine p (World _ mines) = p `elem` mines

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
  | Just (x, y) <- leftClicked, Just _ <- insideRestart x y = world0
  | Just (x, y) <- leftClicked,
    Just p <- insideBoard x y =
      let lost = world0 in if isMine p w then lost else open p w
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

boardWidth :: Int
boardWidth = 9

boardHeight :: Int
boardHeight = 10

drawBoard :: Board -> Picture
drawBoard (Board b) = center $ pictures $ map (\(p, cell) -> at p $ drawCell cell) (assocs b)
  where
    at (CellPos x y) = translate (size x) (size y)
    center = translate (negate . halfSize $ boardWidth - 1) (negate . halfSize $ boardHeight - 1)

insideBoard :: Float -> Float -> Maybe CellPos
insideBoard x y =
  let left = negate $ halfSize boardWidth
      right = halfSize boardWidth
      bottom = negate $ halfSize boardHeight
      top = halfSize boardHeight
   in if left <= x && x <= right && bottom <= y && y <= top then Just $ screen2cell x y else Nothing

restartWidth :: Int
restartWidth = 100

restartHeight :: Int
restartHeight = 50

restartMargin :: Int
restartMargin = 20

insideRestart :: Float -> Float -> Maybe CellPos
insideRestart x y =
  let right = fromIntegral $ restartWidth `div` 2
      left = negate right
      top = fromIntegral $ (windowHeight `div` 2) - restartMargin :: Float
      bottom = top - fromIntegral restartHeight
   in if left <= x && x <= right && bottom <= y && y <= top then Just $ CellPos 0 0 else Nothing

drawRestart :: MonadIO m => m Picture
drawRestart = do
  let h2 = restartHeight `div` 2
      wh2 = windowHeight `div` 2
      c = white
      y = fromIntegral $ wh2 - (h2 + restartMargin)
   in return
        ( color c $
            translate 0 y $
              pictures
                [rectangleWire (fromIntegral restartWidth) (fromIntegral restartHeight)]
        )

windowWidth :: Int
windowWidth = 500

windowHeight :: Int
windowHeight = 500

cellSize :: Float
cellSize = 30

main :: IO ()
main = do
  playIO
    (InWindow "Mine Sweeper" (windowWidth, windowHeight) (10, 10))
    black
    5
    world0
    drawWorld
    (\e w -> return $ event e w)
    (\_ w -> return w)