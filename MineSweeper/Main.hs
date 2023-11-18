{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module Main (main) where

import BitmapFont
import Control.Monad.Reader (Reader, ask, asks, runReader)
import Data.Array.IArray (Array, IArray (bounds), Ix (..), array, assocs, (!), (//))
import Data.Foldable (foldl')
import Graphics.Gloss.Interface.Pure.Game
  ( Color,
    Display (InWindow),
    Event (EventKey),
    Key (MouseButton),
    KeyState (Down, Up),
    Modifiers (Modifiers),
    MouseButton (LeftButton, RightButton),
    Picture (Text),
    black,
    color,
    green,
    greyN,
    line,
    makeColorI,
    mixColors,
    pictures,
    play,
    rectangleSolid,
    rectangleWire,
    red,
    scale,
    translate,
    white,
  )
import Prelude hiding (Left, Right)

data ScreenPos = ScreenPos Float Float deriving (Eq, Ord)

data Count = Zero | One | Two | Three | Four | Five | Six | Seven | Eight deriving (Eq, Enum)

instance Show Count where
  show = show . fromEnum

data Cell = Flaged | Opened Count | UnOpened deriving (Eq)

data CellPos = CellPos Int Int deriving (Eq, Ord, Ix, Show)

newtype Board = Board (Array CellPos Cell)

data WorldState = Playing | Lost | Won deriving (Eq)

data World = World {board :: Board, mines :: [CellPos], state :: WorldState}

data Config = Config {font :: Font}

world0 :: World
world0 = World (Board clean) mines Playing
  where
    _bounds = let lower = CellPos 0 0; upper = CellPos (boardWidth - 1) (boardHeight - 1) in (lower, upper)
    clean = array _bounds $ map (,UnOpened) (range _bounds)
    mines =
      [ CellPos 0 0,
        CellPos 0 1,
        CellPos 1 1,
        CellPos 2 0,
        CellPos 1 2,
        CellPos 1 3,
        CellPos 0 8,
        CellPos 8 0,
        CellPos 2 1,
        CellPos 2 3,
        CellPos 3 3,
        CellPos 4 4,
        CellPos 4 5,
        CellPos 6 5,
        CellPos 5 6
      ]

drawWorld :: World -> Game Picture
drawWorld w@(World b mines st) = do
  restart <- drawRestart st
  minesLeft <- drawMinesLeft w
  board <- if st == Lost then drawBoardLost mines b else drawBoard b
  return $ pictures [board, gridLines b, restart, minesLeft]

drawMinesLeft :: World -> Game Picture
drawMinesLeft (World b mines _) = do
  font <- ask
  let txt = show $ countMinesLeft b $ length mines
      s = 2.0
      dy = -(height s / 2)
      dx = -(width2 s * fromIntegral (length txt) / 2)
      label = color white $ translate dx dy $ scale s s $ drawText font txt
  return $ translate (halfSize boardWidth) (halfSize boardHeight) label

countMinesLeft :: Board -> Int -> Int
countMinesLeft (Board b) total = foldl' (\c cell -> c - f cell) total b
  where
    f Flaged = 1
    f _ = 0

gridLines :: Board -> Picture
gridLines (Board b) = center $ pictures $ draw <$> range (bounds b)
  where
    draw p@(CellPos x y) =
      let s = halfSize (1 :: Integer)
          right = [line [(s, -s), (s, s)] | border $ CellPos (x + 1) y]
          bottom = [line [(-s, -s), (s, -s)] | y == 0 || border (CellPos x (y - 1))]
          left = [line [(-s, s), (-s, -s)] | x == 0 || border (CellPos (x - 1) y)]
          top = [line [(-s, s), (s, s)] | border $ CellPos x (y + 1)]
          border p'
            | inRange (bounds b) p', Opened Zero <- b ! p, Opened Zero <- b ! p' = False
            | otherwise = True
       in color white $ at p $ pictures $ concat [top, right, bottom, left]

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

drawFlag :: Picture
drawFlag = let s = halfSize (1 :: Integer) in color red $ line [(-s, -s), (s, s)] <> line [(-s, s), (s, -s)]

type Game = Reader Font

drawCell :: Cell -> Game Picture
drawCell Flaged = return $ pictures [emptyCell, drawFlag]
drawCell (Opened Zero) = return $ pictures [emptyCell' $ greyN 0.5]
drawCell (Opened c) = (\num -> pictures [emptyCell, num]) <$> number2 c
drawCell UnOpened = return emptyCell

number2 :: Count -> Game Picture
number2 c = asks (number c)

number :: Count -> Font -> Picture
number c font = translate d d $ color (for c) $ scale s s $ drawText font $ show c
  where
    s = 2.0
    d = -cellSize / 2 + 5
    for Zero = white
    for One = makeColorI 0 0 245 255
    for Two = makeColorI 56 128 34 255
    for Three = makeColorI 228 51 36 255
    for Four = makeColorI 0 0 127 255
    for Five = makeColorI 121 21 13 255
    for Six = makeColorI 56 128 130 255
    for Seven = makeColorI 121 12 128 0255
    for Eight = greyN 0.5

background :: Color
background = greyN 0.7

mixWithBackground :: Color -> Color
mixWithBackground = mixColors 0.8 0.2 background

correct :: Color
correct = mixWithBackground green

incorrect :: Color
incorrect = mixWithBackground red

emptyCell :: Picture
emptyCell = emptyCell' background

emptyCell' :: Color -> Picture
emptyCell' = square

square :: Color -> Picture
square c = color c $ rectangleSolid cellSize cellSize

open :: CellPos -> World -> World
open p0 w@(World (Board b0) mines _) = w {board = Board $ go b0 p0}
  where
    go b p
      | Opened _ <- b ! p = b
      | otherwise = do
          let ps = neighbors p
              count = foldl (\c _p -> if _p `elem` mines then succ c else c) Zero ps
              b' = b // [(p, Opened count)]
           in if count /= Zero then b' else foldl go b' ps

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
event e w@World {board = board, mines = mines, state = state}
  | Just (x, y) <- leftClicked, Just _ <- insideRestart x y = world0
  | playing,
    Just (x, y) <- leftClicked,
    Just p <- insideBoard x y =
      let isMine = p `elem` mines
       in if isMine
            then w {state = Lost}
            else let w'@World {board = b'} = open p w in if isWon mines b' then w' {state = Won} else w'
  | playing,
    Just (x, y) <- rightClicked,
    Just pos <- insideBoard x y =
      w {board = flag pos board}
  | otherwise = w
  where
    playing = state == Playing
    clicked
      | EventKey (MouseButton btn) Down (Modifiers Up Up Up) (x, y) <- e = Just (btn, (x, y))
      | otherwise = Nothing
    leftClicked
      | Just (LeftButton, xy) <- clicked = Just xy
      | otherwise = Nothing
    rightClicked
      | Just (RightButton, xy) <- clicked = Just xy
      | otherwise = Nothing

isWon :: [CellPos] -> Board -> Bool
isWon mines (Board b) = all f $ assocs b
  where
    f (_, Opened _) = True
    f (p, _) = p `elem` mines

boardWidth :: Int
boardWidth = 9

boardHeight :: Int
boardHeight = 9

drawBoard' :: (CellPos -> Cell -> Game Picture) -> Board -> Game Picture
drawBoard' draw (Board b) = center . pictures <$> traverse (uncurry draw) (assocs b)

drawBoard :: Board -> Game Picture
drawBoard = drawBoard' (\p c -> at p <$> drawCell c)

drawBoardLost :: [CellPos] -> Board -> Game Picture
drawBoardLost mines = drawBoard' draw
  where
    draw p c =
      let out
            | c == Flaged && p `notElem` mines = return $ mine incorrect
            | c == Flaged && p `elem` mines = return $ mine correct
            | p `elem` mines = return $ mine background
            | otherwise = drawCell c
       in at p <$> out
    mine c = pictures [emptyCell' c, drawFlag]

at :: CellPos -> Picture -> Picture
at (CellPos x y) = translate (size x) (size y)

center :: Picture -> Picture
center = translate (negate . halfSize $ boardWidth - 1) (negate . halfSize $ boardHeight - 1)

insideBoard :: Float -> Float -> Maybe CellPos
insideBoard x y =
  let left = negate $ halfSize boardWidth
      right = halfSize boardWidth
      bottom = negate $ halfSize boardHeight
      top = halfSize boardHeight
   in if left <= x && x <= right && bottom <= y && y <= top then Just $ screen2cell x y else Nothing

restartWidth :: Int
restartWidth = 150

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

drawRestart :: WorldState -> Game Picture
drawRestart st =
  let h2 = restartHeight `div` 2
      wh2 = windowHeight `div` 2
      (c, txt) = case st of
        Playing -> (greyN 0.5, "RESTART")
        Lost -> (red, "LOST")
        Won -> (green, "WON")
      y = fromIntegral $ wh2 - (h2 + restartMargin)
   in do
        font <- ask
        let button = rectangleWire (fromIntegral restartWidth) (fromIntegral restartHeight)
            ss = 2.0
            dy = -(height ss / 2)
            dx = -(width2 ss * 8 / 2)
            label = translate dx dy $ scale ss ss $ drawText font txt
        return $ color c $ translate 0 y $ pictures [button, label]

windowWidth :: Int
windowWidth = 500

windowHeight :: Int
windowHeight = 500

cellSize :: Float
cellSize = 35

main :: IO ()
main = do
  font <- readFont "unifont_all-15.1.02.hex"
  play
    (InWindow "Mine Sweeper" (windowWidth, windowHeight) (10, 10))
    black
    5
    world0
    (\w -> runReader (drawWorld w) font)
    event
    (\_ w -> w)