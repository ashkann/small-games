{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

import Data.Array
import Debug.Trace (trace)
import Graphics.Gloss hiding (rotet)
import Graphics.Gloss.Interface.Pure.Game hiding (rotate)
import TicTacToe qualified as T3

data Tetromino = I | O | T | J | L | S | Z deriving (Eq, Show, Bounded, Enum)

newtype Col = Col Int deriving (Eq, Ord, Show, Ix, Num)

instance Bounded Col where
  minBound = Col 1
  maxBound = Col 10

instance Enum Col where
  fromEnum (Col c) = c
  enumFrom (Col l) = let Col u = maxBound in [Col x | x <- [l .. u]]

newtype Row = Row Int deriving (Eq, Ord, Show, Ix, Num)

instance Bounded Row where
  minBound = Row 1
  maxBound = Row 16

instance Enum Row where
  fromEnum (Row r) = r
  enumFrom (Row l) = let Row u = maxBound in [Row x | x <- [l .. u]]

data Pos = Pos Row Col deriving (Eq, Ord, Show, Ix)

instance Bounded Pos where
  minBound = Pos minBound minBound
  maxBound = Pos maxBound maxBound

instance Enum Pos where
  enumFrom (Pos rl cl) = [Pos r c | r <- [rl ..], c <- [cl ..]]

newtype Measure = Measure Int deriving (Eq, Num)

halfBlocks :: Int -> Measure
halfBlocks = Measure

fullBlocks :: Int -> Measure
fullBlocks = Measure . (* 2)

newtype Cell = Cell (Maybe Tetromino) deriving (Show)

newtype Playfield = Playfield (Array Pos Cell)

data World = World
  { pointer :: Point,
    playfield :: Playfield
  }

data Rotation = R0 | R1 | R2 | R3

data BoxCell = A | X

data Box
  = Box3x3
      (BoxCell, BoxCell, BoxCell)
      (BoxCell, BoxCell, BoxCell)
      (BoxCell, BoxCell, BoxCell)
  | Box4x4
      (BoxCell, BoxCell, BoxCell, BoxCell)
      (BoxCell, BoxCell, BoxCell, BoxCell)
      (BoxCell, BoxCell, BoxCell, BoxCell)
      (BoxCell, BoxCell, BoxCell, BoxCell)

data SRS = SRS {r0 :: Box, r1 :: Box, r2 :: Box, r3 :: Box}

rotate' :: Box -> Box
rotate'
  ( Box3x3
      (c00, c01, c02)
      (c10, c11, c12)
      (c20, c21, c22)
    ) =
    Box3x3
      (c20, c10, c00)
      (c21, c11, c01)
      (c22, c12, c02)
rotate'
  ( Box4x4
      (c00, c01, c02, c03)
      (c10, c11, c12, c13)
      (c20, c21, c22, c23)
      (c40, c31, c32, c33)
    ) =
    Box4x4
      (c00, c01, c02, c03)
      (c10, c11, c12, c13)
      (c20, c21, c22, c23)
      (c40, c31, c32, c33)

data I = I1 | I2 | I3 | I4

instance Enum I where
  fromEnum I1 = 0
  fromEnum I2 = 1
  fromEnum I3 = 2
  fromEnum I4 = 3

data B = B I I

data Tet = Tet B B B B

tetShape :: Tetromino -> Tet
tetShape S = Tet b21 b22 b12 b13
tetShape I = Tet b11 b12 b13 b14
tetShape O = Tet b11 b12 b21 b22
tetShape T = Tet b11 b12 b13 b22
tetShape J = Tet b11 b12 b13 b21
tetShape L = Tet b21 b11 b12 b13
tetShape Z = Tet b11 b12 b22 b23

tetColor :: Tetromino -> Color
tetColor S = green
tetColor I = cyan
tetColor O = yellow
tetColor T = makeColor 0.627 0.125 0.941 1.0
tetColor J = blue
tetColor L = orange
tetColor Z = red

b11 = B I1 I1

b12 = B I1 I2

b13 = B I1 I3

b14 = B I1 I4

b21 = B I2 I1

b22 = B I2 I2

b23 = B I2 I3

b24 = B I2 I4

b31 = B I3 I1

b32 = B I3 I2

b33 = B I3 I3

b34 = B I3 I4

b41 = B I4 I1

b42 = B I4 I2

b43 = B I4 I3

b44 = B I4 I4

size :: Measure -> Int
size (Measure n) = let halfSize = 17 in n * halfSize

fsize :: Measure -> Float
fsize = fromIntegral . size

put :: Pos -> Tetromino -> Playfield -> Playfield
put (Pos r c) t (Playfield p) = Playfield $ p // (es <$> [s1, s2, s3, s4])
  where
    Tet s1 s2 s3 s4 = tetShape t
    es b = (pos b, Cell . Just $ t)
    pos (B br bc) =
      let r' = r + Row (fromEnum br)
          c' = c + Col (fromEnum bc)
       in Pos r' c'

main :: IO ()
-- main = let Playfield cells = emptyPlayfield in print $ bounds cells
main = play d backgroundColor 1 world2 draw event (const id)
  where
    emptyPlayfield = Playfield $ array (minBound, maxBound) [(pos, Cell Nothing) | pos <- [minBound ..]]
    emptyWorld = World {pointer = (0, 0), playfield = emptyPlayfield}
    (pf, _) = let f (pf, pos@(Pos r c)) t = (put pos t pf, Pos (r + Row 2) c) in foldl f (emptyPlayfield, Pos (Row 1) (Col 1)) [minBound..maxBound]
    world2 = World {pointer = (0, 0), playfield = pf }
    d = InWindow "Tic-Tac-Toe" (windowSize, windowSize) (100, 100)
    windowSize = 700 :: Int
    backgroundColor = makeColor 0 0 0 0
    gridColor = makeColor 1 1 1 1
    margin = 20
    cw = fromIntegral $ (windowSize - 2 * margin) `div` 6
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
    px = Pos (Row 1) (Col 1)
    -- Drawing the game
    draw World {pointer = (x, y), playfield = playfield} =
      Pictures
        [ color gridColor $ circle 5,
          drawPlayfield playfield
          -- drawWalls
        ]
      where
        height = let Row x = maxBound - minBound in x + 1
        width = let Col x = maxBound - minBound in x + 1
        at r c =
          let dx = fsize $ fullBlocks c - halfBlocks (width + 1)
              dy = fsize $ fullBlocks r - halfBlocks (height + 1)
           in translate dx dy
        atPos (Pos (Row r) (Col c)) = at r c
        drawEmptyCell =
          let c = makeColor 1 1 1 0.05
              padding = 2
              s = fromIntegral $ (size . fullBlocks $ 1) - padding
           in color c $ rectangleSolid s s
        drawPlayfield (Playfield cells) = pictures $ foldl drw [] (assocs cells)
          where
            drw pics (pos, Cell (Just t)) = atPos pos (drawTet t) : pics
            drw pics (pos, Cell Nothing) = atPos pos drawEmptyCell : pics
        drawTet t = color (tetColor t) $ drawRect 1 1
        drawWalls =
          let c = makeColor 0.5 0.5 0.5 1
              h = color c $ drawRect (width + 2) 1
              v = color c $ drawRect 1 height
           in pictures [at 0 1 v, at 0 0 h, at (width + 1) 1 v]
        drawRect w h =
          let dx = fsize . halfBlocks $ w - 1
              dy = fsize . halfBlocks $ h - 1
              center = translate dx dy
           in center $ rectangleSolid (fsize . fullBlocks $ w) (fsize . fullBlocks $ h)
