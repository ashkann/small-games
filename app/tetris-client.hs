{-# LANGUAGE ImportQualifiedPost #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

import Data.Array
import Debug.Trace (trace)
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import TicTacToe qualified as T3

data Tetromino = I | O | T | J | L | S | Z deriving (Eq, Show)

newtype Col = Col Int deriving (Eq, Ord, Show, Ix)

instance Bounded Col where
  minBound = Col 1
  maxBound = Col 10

instance Enum Col where
  fromEnum (Col c) = c
  enumFrom (Col l) = let Col u = maxBound in [Col x | x <- [l .. u]]

newtype Row = Row Int deriving (Eq, Ord, Show, Ix)

instance Bounded Row where
  minBound = Row 1
  maxBound = Row 16

instance Enum Row where
  fromEnum (Row r) = r
  enumFrom (Row l) = let Row u = maxBound in [Row x | x <- [l .. u]]

data Pos = Pos Col Row deriving (Eq, Ord, Show, Ix)

instance Bounded Pos where
  minBound = Pos minBound minBound
  maxBound = Pos maxBound maxBound

instance Enum Pos where
  enumFrom (Pos cl rl) = [Pos c r | r <- [rl ..], c <- [cl ..]]

newtype Cell = Cell (Maybe Tetromino) deriving Show

newtype Playfield = Playfield (Array Pos Cell)

data World = World
  { pointer :: Point,
    playfield :: Playfield
  }

data Rotation = Zero | One | Two | Three

-- data Row = Row Bool Bool Bool Bool

-- data Tet = Tet Row Row Row Row

-- tet :: Tetromino -> Tet
-- tet t
--   | t == I =
--       Tet
--         xxxx
--         ____0
--         ____0
--         ____0
--   | t == O =
--       Tet
--         xxxx
--         xxxx
--         xxxx
--         xxxx
--   | t == T =
--       Tet
--         xxx
--         _x_
--         ____0
--         ____0
--   where
--     xxxx = Row True True True False
--     xxx = Row True True True False
--     _x_ = Row True True True False
--     ____0 = Row False False False False

-- draw :: Tetromino -> Rotation -> Picture
-- draw I r = rectangleSolid () _
--     where blockSize = 10

main :: IO ()
-- main = let Playfield cells = emptyPlayfield in print $ bounds cells
main = play d backgroundColor 1 emptyWorld draw event (const id)
  where
    emptyPlayfield = Playfield $ array (minBound, maxBound) [(pos, Cell Nothing) | pos <- [minBound ..]]
    emptyWorld = World {pointer = (0, 0), playfield = emptyPlayfield}
    d = InWindow "Tic-Tac-Toe" (size, size) (100, 100)
    size = 700 :: Int
    backgroundColor = makeColor 0 0 0 0
    gridColor = makeColor 1 1 1 1
    margin = 20
    cw = fromIntegral $ (size - 2 * margin) `div` 6
    -- Event handling
    event ev w
      | pressedX = w
      | pressedO = w
      | (EventMotion point) <- ev = w {pointer = point}
      | otherwise = w
      where
        pressedX
          | EventKey (Char 'x') Down (Modifiers Up Up Up) _ <- ev = True
          | otherwise = False
        pressedO
          | EventKey (Char 'o') Down (Modifiers Up Up Up) _ <- ev = True
          | otherwise = False
        clicked
          | EventKey (MouseButton LeftButton) Down (Modifiers Up Up Up) (x, y) <- ev = Just (x, y)
          | otherwise = Nothing
        toPos x y = (toIndex y, toIndex x)
          where
            toIndex x
              | x <= -cw = T3.I1
              | x <= cw = T3.I2
              | otherwise = T3.I3
    -- Drawing the game
    draw World {pointer = (x, y), playfield = playfield} =
      Pictures
        [
          color gridColor $ circle 5,
          drawPlayfield playfield
        ]
      where
        size = 40 :: Int
        at (Pos (Col c) (Row r)) =
            let cols = (fromEnum (maxBound :: Col) - fromEnum (minBound :: Col)) `div` 2
                rows = (fromEnum (maxBound :: Row) - fromEnum (minBound :: Row)) `div` 2
                -- z = trace ("x=" ++ show xOffset ++ "y=" ++ show yOffset) 0
                xOffset = (c - cols) * size
                yOffset = (r - rows) * size 
            in translate (fromIntegral xOffset) (fromIntegral yOffset)
        drawEmptyCell =
          let c = makeColor 1 1 1 0.05
              padding = 2
           in color c (rectangleSolid (fromIntegral size - padding) (fromIntegral size - padding))
        drawPlayfield (Playfield cells) = pictures $ (\(pos, _) -> at pos drawEmptyCell) <$> assocs cells
