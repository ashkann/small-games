{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Data.Type.Coercion (trans)
import Graphics.Gloss.Interface.Pure.Game
  ( Color,
    Display (InWindow),
    Event,
    Path,
    Picture,
    black,
    blank,
    color,
    cyan,
    dark,
    dim,
    green,
    line,
    makeColor,
    orange,
    pictures,
    play,
    polygon,
    red,
    rgbaOfColor,
    translate,
    yellow,
  )

windowWidth :: Int
windowWidth = 500

windowHeight :: Int
windowHeight = 500

newtype World = World Float

tick :: Float -> World -> World
tick dt (World t) = World $ t + dt

event :: Event -> World -> World
event e w = w

data LED = LED
  { drawOn :: Segment -> Picture,
    drawOff :: Segment -> Picture,
    at :: Int -> Picture -> Picture
  }

data Segment = A | B | C | D | E | F | G

thinLED :: Float -> Float -> Color -> LED
thinLED size spacing c =
  LED
    { drawOn = draw c,
      drawOff = draw $ (dark . dark . dark . dark) c,
      at = at'
    }
  where
    at' x = translate (fromIntegral x * (size + spacing)) 0
    size2 = size * 2
    draw c seg = color c $ line $ path seg
    path A = [(0, size2), (size, size2)]
    path B = [(size, size2), (size, size)]
    path C = [(size, size), (size, 0)]
    path D = [(size, 0), (0, 0)]
    path E = [(0, 0), (0, size)]
    path F = [(0, size), (0, size2)]
    path G = [(0, size), (size, size)]

thickLED :: Float -> Float -> Float -> Color -> LED
thickLED w h spacing c =
  LED
    { drawOn = draw c,
      drawOff = draw cOff,
      at = at'
    }
  where
    cOff = let (r, g, b, a) = rgbaOfColor c in makeColor (r - 0.87) (g - 0.87) (b - 0.87) a
    at' x = let size = ((w + h + g + h) * 2) + spacing in translate (fromIntegral x * size) 0
    draw c seg = color c $ poly seg
    v = polygon [(0, -(w + h)), (-h, -w), (-h, w), (0, w + h), (h, w), (h, -w)]
    g = 1
    d = h + w + g
    d2 = d * 2
    poly A = translate 0 d2 $ poly G
    poly B = translate d d v
    poly C = translate d (-d) v
    poly D = translate 0 (-d2) $ poly G
    poly E = translate (-d) (-d) v
    poly F = translate (-d) d v
    poly G = polygon [(-(w + h), 0), (-w, h), (w, h), (w + h, 0), (w, -h), (-w, -h)]

type SevenSegment = (Bool, Bool, Bool, Bool, Bool, Bool, Bool)

data Digit = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine

toSevenSegment :: Digit -> SevenSegment
toSevenSegment Zero = (True, True, True, True, True, True, False)
toSevenSegment One = (False, True, True, False, False, False, False)
toSevenSegment Two = (True, True, False, True, True, False, True)
toSevenSegment Three = (True, True, True, True, False, False, True)
toSevenSegment Four = (False, True, True, False, False, True, True)
toSevenSegment Five = (True, False, True, True, False, True, True)
toSevenSegment Six = (True, False, True, True, True, True, True)
toSevenSegment Seven = (True, True, True, False, False, False, False)
toSevenSegment Eight = (True, True, True, True, True, True, True)
toSevenSegment Nine = (True, True, True, True, False, True, True)

drawDigit :: LED -> Digit -> Picture
drawDigit led = drawSevenSegment led . toSevenSegment

drawSevenSegment :: LED -> SevenSegment -> Picture
drawSevenSegment led (a, b, c, d, e, f, g) =
  pictures
    [ draw a A,
      draw b B,
      draw c C,
      draw d D,
      draw e E,
      draw f F,
      draw g G
    ]
  where
    draw isOn seg = if isOn then drawOn led seg else drawOff led seg

drawClock :: World -> Picture
drawClock (World t) =
  let t' = show (round $ t * 10 :: Int)
      led1 = thinLED 10 5 yellow
      led2 = thinLED 10 10 orange
      led3 = thinLED 10 15 green
      led4 = thinLED 10 2 cyan
      led5 = thickLED 15 5 10 cyan
      toDigit = \case
        '0' -> Zero
        '1' -> One
        '2' -> Two
        '3' -> Three
        '4' -> Four
        '5' -> Five
        '6' -> Six
        '7' -> Seven
        '8' -> Eight
        '9' -> Nine
        _ -> error "Invalid digit"
      digits = toDigit <$> t'
      draw led = pictures $ zipWith (\x d -> at led x $ drawDigit led d) [0 ..] digits
   in pictures
        [ translate 0 50.0 $ draw led1,
          translate 0 0 $ draw led2,
          -- translate 0 (-30) $ draw led3,
          -- translate 0 (-60) $ draw led4,
          translate 0 (-50) $ draw led5
        ]

main :: IO ()
main = do
  play
    (InWindow "Seven Segment Display" (windowWidth, windowHeight) (10, 10))
    black
    20
    (World 0.0)
    drawClock
    event
    tick
