{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE UndecidableInstances #-}

module Main (main) where

import Data.Maybe (maybeToList)
import Graphics.Gloss.Interface.Pure.Game (Display (InWindow), Event (EventKey), Key (SpecialKey), Picture, SpecialKey (KeyDown, KeyLeft, KeyRight, KeyUp), black, color, pictures, play, rectangleSolid, rectangleWire, translate, white)
import Graphics.Gloss.Interface.Pure.Game qualified as G (KeyState (Down))
import Prelude hiding (Left, Right)

class Sized a where
  unwrap :: a -> Double
  size :: a -> Double
  size = (* blockSize) . unwrap
  sizef :: a -> Float
  sizef = realToFrac . size

newtype Length = Length Double deriving (Num)

newtype Block = Block Int deriving (Num)

instance Sized Length where
  unwrap (Length x) = x

instance Sized Block where
  unwrap (Block x) = fromIntegral x

fieldWidth :: Block
fieldWidth = Block 20

fieldHeight :: Block
fieldHeight = Block 20

blockSize :: Double
blockSize = fromIntegral windowWidth / fromIntegral w where Block w = fieldWidth

data Direction = Up | Down | Left | Right

data Segment = Segment Direction Length

data Snake = Snake Segment [Segment] (Maybe Segment)

data Position = Position Double Double

drawSnake :: Snake -> Picture
drawSnake (Snake head body tail) = pictures $ segment (Length 0) (Length 0) ss []
  where
    ss = head : body ++ maybeToList tail
    horizontal w = rectangleWire (sizef w) (sizef $ Length 1)
    vertical h = rectangleWire (sizef $ Length 1) (sizef h)
    segment _ _ [] segs = segs
    segment x y ((Segment d l) : xs) segs =
      let (x', y', seg) = case d of
            Up -> (x, y + l, vertical l)
            Down -> (x, y - l, vertical l)
            Left -> (x - l, y, horizontal l)
            Right -> (x + l, y, horizontal l)
       in segment x' y' xs (seg : segs)

newtype Velocity = Velocity Float

data World = World Snake Position Velocity Direction

world0 :: World
world0 = World snake0 pos0 velocity0 Left
  where
    pos0 = Position 0 0
    snake0 = Snake (Segment Right (Length 3.0)) [Segment Up (Length 2.0), Segment Left (Length 3.34)] Nothing
    velocity0 = Velocity 1

drawWorld :: World -> Picture
drawWorld (World snake p v d) =
  pictures [color white $ drawSnake snake, drawVelocity, drawDirection]
  where
    drawVelocity = pictures []
    drawDirection = color white $ case d of
      Up -> rectangleSolid (sizef fieldWidth) 1
      Down -> rectangleSolid (sizef fieldWidth) 1
      Left -> rectangleSolid 1 (sizef fieldHeight)
      Right -> rectangleSolid 1 (sizef fieldHeight)

update :: Float -> World -> World
update dt (World s p v d) = World s' p v d
  where
    dx = let Velocity v' = v in dt * v'
    -- Snake hd (d0, l0) ss = s
    s' = s

event :: Event -> World -> World
event e w@(World s p v _)
  | Just k <- special e = case k of
      KeyUp -> World s p v Up
      KeyDown -> World s p v Down
      KeyLeft -> World s p v Left
      KeyRight -> World s p v Right
      _ -> w
  | otherwise = w
  where
    special (EventKey (SpecialKey k) G.Down _ _) = Just k
    special _ = Nothing

windowWidth :: Int
windowWidth = 400

windowHeight :: Int
windowHeight = 400

main :: IO ()
main =
  play
    (InWindow "Snake" (windowWidth, windowHeight) (10, 10))
    black
    30
    world0
    drawWorld
    event
    update
