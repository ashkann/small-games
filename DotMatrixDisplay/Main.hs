{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State.Lazy
import Data.Bits (Bits ((.&.)), shiftL, testBit, (.|.))
import Data.Char (chr, ord)
import Data.Function ((&))
import Data.Functor.Identity (Identity (Identity))
import Data.Map.Strict (insert)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Type.Coercion (trans)
import Data.Word (Word16, Word8)
import Debug.Trace (trace)
import GHC.Base ((<|>))
import Graphics.Gloss.Interface.Pure.Game
  ( Color,
    Display (InWindow),
    Path,
    Picture,
    SpecialKey (KeyAltL, KeyBackspace),
    black,
    blank,
    blue,
    circle,
    circleSolid,
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
    rectangleSolid,
    red,
    rgbaOfColor,
    translate,
    yellow,
  )
import Graphics.Gloss.Interface.Pure.Game qualified as G
import Streamly.Data.Array qualified as Array
import Streamly.Data.Fold qualified as Fold
import Streamly.Data.Stream qualified as Stream
import Streamly.FileSystem.File qualified as File

windowWidth :: Int
windowWidth = 1000

windowHeight :: Int
windowHeight = 500

data Input = Input {s :: String, cursor :: Glyph}

data World = World {font :: Font, time :: Float, input :: Input}

tick :: Float -> World -> World
tick dt w@(World {time = t}) = w {time = t + dt}

data KeyboardEvent = Typed Char | MoveLeft | MoveRight | DeleteLeft

updateInput :: Input -> KeyboardEvent -> Input
updateInput i@(Input s _) ev = case ev of
  Typed ch -> i {s = s ++ [ch]}
  MoveLeft -> i
  MoveRight -> i
  DeleteLeft -> if null s then i else i {s = init s}

event :: G.Event -> World -> World
event e w@(World font t i)
  | Just ev <- keyboard = w {input = updateInput i ev}
  | otherwise = w
  where
    keyboard
      | G.EventKey (G.Char ch) G.Down (G.Modifiers _ G.Up G.Up) _ <- e = Just $ Typed ch
      | G.EventKey (G.SpecialKey G.KeyDelete) G.Down (G.Modifiers G.Up G.Up G.Up) _ <- e = Just DeleteLeft
      | otherwise = Nothing

draw :: World -> Picture
draw (World font t (Input {s = s})) =
  let l1 = "Seconds passed: " ++ show (floor t :: Int)
      l2 = "ABCDEFGHIJKLMNOP±QRSTUVWXYZ 1234567890"
      l3 = "abcdefghijklmnopqrstuvwxyz !@#$%^&*()_+ ±"
      l4 = s
      s1 = sqaureDotStyle blue 1 0
      s2 = sqaureDotStyle blue 2 0
      s3 = sqaureDotStyle green 2 2
      s4 = sqaureDotStyle yellow 2 0
      lines = [(l1, s4), (l2, s2), (l3, s1), (l4, s3)]
      x = negate . fromIntegral $ windowWidth `div` 2
      magic (str, style) = do
        y <- get
        _ <- put $ y + height style
        return $ translate x y (drawText style font str)
   in pictures $ evalState (mapM magic lines) 0

drawText :: Style -> Font -> String -> Picture
drawText style font s = pictures $ zipWith (\x c -> atChar style x $ drawChar style font c) [1 :: Int ..] s

drawChar :: Style -> Font -> Char -> Picture
drawChar style font c = case Map.lookup c font <|> glyph2 replacement of
  Just g -> drawGlyph style g
  Nothing -> blank
  where
    replacement = [0x00, 0x00, 0x00, 0x7E, 0x66, 0x5A, 0x5A, 0x7A, 0x76, 0x76, 0x7E, 0x76, 0x76, 0x7E, 0x00, 0x00]

drawGlyph :: Style -> Glyph -> Picture
drawGlyph style (Glyph r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15) =
  pictures $ zipWith (\y r -> atY style y $ drawRow style r) [1 :: Int ..] rows
  where
    rows = [r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15]

drawRow :: Style -> Row -> Picture
drawRow style (Row b0 b1 b2 b3 b4 b5 b6 b7) =
  pictures $ zipWith (\x b -> atX style x $ bit b) [1 :: Int ..] bits
  where
    bit On = on style
    bit Off = off style
    bits = [b0, b1, b2, b3, b4, b5, b6, b7]

data Style = Style
  { on :: Picture,
    off :: Picture,
    atX :: Int -> Picture -> Picture,
    atY :: Int -> Picture -> Picture,
    atChar :: Int -> Picture -> Picture,
    width :: Float,
    height :: Float
  }

-- data Style

sqaureDotStyle :: Color -> Float -> Float -> Style
sqaureDotStyle c size space =
  let dot = rectangleSolid size size
      s i = fromIntegral i * (size + space)
      dotsPerRow = 8
      rowsPerGlyph = 16
      w = s dotsPerRow
   in Style
        { on = color c dot,
          off = color (dark . dark . dark . dark $ c) dot,
          atX = \x -> translate (s x) 0,
          atY = \y -> translate 0 (s y),
          atChar = \i -> translate (fromIntegral i * w) 0,
          width = w,
          height = s rowsPerGlyph
        }

-- strWidth :: Style -> String -> Float
-- strWidth style str =

data Bit = On | Off deriving (Enum, Show)

data Row = Row Bit Bit Bit Bit Bit Bit Bit Bit deriving (Show)

row0 :: Row
row0 = Row Off Off Off Off Off Off Off Off

row1 :: Row
row1 = Row On On On On On On On On

data Glyph = Glyph Row Row Row Row Row Row Row Row Row Row Row Row Row Row Row Row deriving (Show)

type Font = Map.Map Char Glyph

glyph2 :: [Word8] -> Maybe Glyph
glyph2 hex = rows $ go hex [] (16 :: Int)
  where
    go (c0 : rest) acc n | n /= 0 = let r = row c0 in go rest (r : acc) (n - 1)
    go _ acc _ = acc
    row c = Row (at 7) (at 6) (at 5) (at 4) (at 3) (at 2) (at 1) (at 0)
      where
        at i = if testBit c i then On else Off
    rows [r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15] = Just $ Glyph r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15
    rows _ = Nothing

glyph :: [Word8] -> Maybe Glyph
glyph hex = fromRow $ go hex [] (16 :: Int)
  where
    go (c0 : c1 : rest) rows n | n /= 0 = let r = row $ byte c0 c1 in go rest (r : rows) (n - 1)
    go _ rows _ = rows
    row :: Word8 -> Row
    row c = Row (at 7) (at 6) (at 5) (at 4) (at 3) (at 2) (at 1) (at 0)
      where
        at i = if testBit c i then On else Off
    fromRow [r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15] = Just $ Glyph r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15
    fromRow _ = Nothing

byte :: Word8 -> Word8 -> Word8
byte hi lo = (hexDigit hi `shiftL` 4) .|. (hexDigit lo .&. 0x0F)
  where
    hexDigit :: Word8 -> Word8
    hexDigit ch = case toEnum . fromIntegral $ ch of
      '0' -> 0
      '1' -> 1
      '2' -> 2
      '3' -> 3
      '4' -> 4
      '5' -> 5
      '6' -> 6
      '7' -> 7
      '8' -> 8
      '9' -> 9
      'A' -> 10
      'B' -> 11
      'C' -> 12
      'D' -> 13
      'E' -> 14
      'F' -> 15
      _ -> error $ "invalid hex digit: " ++ show (toEnum . fromIntegral $ ch :: Char)

readFont :: (MonadIO m, MonadCatch m) => String -> m Font
readFont name =
  let readLines = Stream.foldMany (Fold.takeEndBy_ (== newLine) Fold.toList) where newLine = 0x0A
      glyphs = Stream.mapMaybe parse $ readLines (File.read name)
      parse :: [Word8] -> Maybe (Char, Glyph)
      parse (c0 : c1 : c2 : c3 : _ : rest)
        | Just g <- glyph rest = Just (toEnum . fromIntegral $ code c0 c1 c2 c3, g)
        | otherwise = Nothing
      parse _ = Nothing
      code c0 c1 c2 c3 =
        let hi = fromIntegral $ byte c0 c1 :: Word16
            lo = fromIntegral $ byte c2 c3 :: Word16
         in hi `shiftL` 8 .|. (lo .&. 0x00FF)
      font = Fold.foldl' (\m (k, v) -> insert k v m) Map.empty
   in Stream.fold font $ Stream.take 128 glyphs

game :: Font -> IO ()
game font =
  play
    (InWindow "Dot Matrix Display" (windowWidth, windowHeight) (10, 10))
    black
    20
    (World font 0.0 (Input "" (Glyph row0 row0 row0 row0 row0 row0 row0 row0 row0 row0 row0 row0 row0 row0 row0 row1)))
    draw
    event
    tick

main :: IO ()
main = do
  font <- readFont "unifont_all-15.1.02.hex"
  game font
