{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

import Data.Array
import Debug.Trace (trace)
import Graphics.Gloss hiding (rotet)
import Graphics.Gloss.Interface.Pure.Game hiding (rotate)

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
  maxBound = Row 20

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
  { playfield :: Playfield,
    falling :: Maybe (Tetromino, Pos, Orientation),
    sky :: [Tetromino]
  }

data Orientation = R0 | R1 | R2 | R3 deriving (Eq, Enum)

data I = I1 | I2 | I3 | I4 deriving (Eq, Ord)

instance Enum I where
  fromEnum I1 = 0
  fromEnum I2 = 1
  fromEnum I3 = 2
  fromEnum I4 = 3

data B = B I I

data Tet = Tet B B B B

data Shape = Shape {r0 :: Tet, r1 :: Tet, r2 :: Tet, r3 :: Tet}

shape :: Tetromino -> Shape
shape t = case t of
  I ->
    let s0 = Tet b11 b12 b13 b14
        s1 = Tet b11 b21 b31 b41
        s2 = s0
        s3 = s1
     in Shape s0 s1 s2 s3
  O ->
    let s0 = Tet b11 b12 b21 b22
        s1 = s0
        s2 = s0
        s3 = s0
     in Shape s0 s1 s2 s3
  S ->
    let s0 = Tet b21 b22 b12 b13
        s1 = Tet b11 b21 b22 b32
        s2 = s0
        s3 = s1
     in Shape s0 s1 s2 s3
  Z ->
    let s0 = Tet b11 b12 b22 b23
        s1 = Tet b12 b22 b21 b31
        s2 = s0
        s3 = s1
     in Shape s0 s1 s2 s3
  T ->
    let s0 = Tet b21 b22 b23 b12
        s1 = Tet b11 b21 b31 b22
        s2 = Tet b11 b12 b13 b22
        s3 = Tet b12 b22 b32 b21
     in Shape s0 s1 s2 s3
  J ->
    let s0 = Tet b11 b21 b22 b23
        s1 = Tet b12 b11 b21 b31
        s2 = Tet b11 b12 b13 b23
        s3 = Tet b31 b32 b22 b12
     in Shape s0 s1 s2 s3
  L ->
    let s0 = Tet b21 b22 b23 b13
        s1 = Tet b11 b21 b31 b32
        s2 = Tet b21 b11 b12 b13
        s3 = Tet b11 b12 b22 b32
     in Shape s0 s1 s2 s3
  where
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

tetColor :: Tetromino -> Color
tetColor S = green
tetColor I = cyan
tetColor O = yellow
tetColor T = makeColor 0.627 0.125 0.941 1.0
tetColor J = blue
tetColor L = orange
tetColor Z = red

gray :: Color
gray = makeColor 0.5 0.5 0.5 1.0

size :: Measure -> Int
size (Measure n) = let halfSize = 17 in n * halfSize

fsize :: Measure -> Float
fsize = fromIntegral . size

put :: Pos -> Orientation -> Tetromino -> Playfield -> Playfield
put (Pos r c) ori t (Playfield p) = Playfield $ p // (es <$> [s1, s2, s3, s4])
  where
    Tet s1 s2 s3 s4 =
      let selector = case ori of
            R0 -> r0
            R1 -> r1
            R2 -> r2
            R3 -> r3
       in selector $ shape t
    es b = (pos b, Cell . Just $ t)
    pos (B br bc) =
      let r' = r - Row (fromEnum br)
          c' = c + Col (fromEnum bc)
       in Pos r' c'

main :: IO ()
-- main = let Playfield cells = emptyPlayfield in print $ bounds cells
main = play d backgroundColor 1 world draw event (const id)
  where
    sky = [I, J, L, O, S, T, Z] ++ sky
    world =
      let emptyPlayfield = Playfield $ array (minBound, maxBound) [(pos, Cell Nothing) | pos <- [minBound ..]]
          emptyWorld = World {playfield = emptyPlayfield, sky = sky, falling = Nothing}
       in spawn emptyWorld
    pf3 r =
      let i = put (Pos (Row 4) (Col 1)) r I
          s = put (Pos (Row 4) (Col 6)) r S
          z = put (Pos (Row 8) (Col 1)) r Z
          t = put (Pos (Row 8) (Col 6)) r T
          l = put (Pos (Row 12) (Col 1)) r L
          j = put (Pos (Row 12) (Col 6)) r J
          o = put (Pos (Row 16) (Col 1)) r O
       in i . s . z . t . l . j . o . i

    spawn World {sky = []} = error "Nothing to spawn !"
    spawn World {playfield = pf, sky = t : sky'} =
      let pos = Pos (Row 20) (Col 5)
          ori = R0
       in World {playfield = put pos ori t pf, falling = Just (t, pos, ori), sky = sky'}

    d = InWindow "Tic-Tac-Toe" (windowSize, windowSize) (100, 100)
    windowSize = 700 :: Int
    backgroundColor = makeColor 0 0 0 0
    gridColor = makeColor 1 1 1 1
    margin = 20
    cw = fromIntegral $ (windowSize - 2 * margin) `div` 6
    -- Event handling
    rotateForward R3 = R0
    rotateForward r = succ r
    rotateBackward R0 = R3
    rotateBackward r = pred r

    updateWorld _ w@World {falling = Nothing} = w
    updateWorld f w@World {playfield = pf, falling = Just (tet, pos, ori)} = w {playfield = pf, falling = Just (tet, pos, f ori)}

    -- let ori = f $ orientation w
    --     pf = pf3 ori emptyPlayfield
    --  in w {playfield = pf}
    event ev w
      | key 'w' = updateWorld rotateForward w
      | key 'e' = updateWorld rotateBackward w
      | key 'x' = spawn w
      -- \| (EventMotion point) <- ev = w {pointer = point}
      | otherwise = w
      where
        key ch
          | EventKey (Char ch') Down (Modifiers Up Up Up) _ <- ev = ch == ch'
          | otherwise = False
    -- clicked
    --   | EventKey (MouseButton LeftButton) Down (Modifiers Up Up Up) (x, y) <- ev = Just (x, y)
    --   | otherwise = Nothing
    -- Drawing the game
    draw World {playfield = playfield} =
      Pictures
        [ -- color gridColor $ circle 5,
          drawPlayfield playfield
          -- drawSky sky
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
        -- drawSky ts = let y = fsize . halfBlocks $ height in color gray (pictures $ go y [] (take 21 ts))
        --   where
        --     go y pics (t : ts) = let (p, dy) = pic t in go (y - dy) (p : pics) ts
        --       where
        --         spacing = size * 0.25
        --         pic t = let tet = tetShape0 t in (drw tet, height tet + spacing)
        --         height (Tet s1 s2 s3 s4) = let rs = [r | B r _ <- [s1, s2, s3, s4]] in (p . maximum $ rs) - (p . minimum $ rs) + size
        --         drw (Tet s1 s2 s3 s4) = pictures $ (`at` rect) <$> [s1, s2, s3, s4]
        --         p i = size * (fromIntegral . fromEnum $ i)
        --         rect = rectangleSolid size size
        --         at (B r c) = let x = fsize . halfBlocks $ width + 4 in translate (p c - x) (y - p r)
        --         size = fsize . halfBlocks $ 1
        --     go _ pics [] = pics
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
