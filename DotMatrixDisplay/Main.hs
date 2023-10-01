{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Data.Bits (Bits ((.&.)), shiftL, testBit, (.|.))
import Data.Char (chr, ord)
import Data.Function ((&))
import Data.Functor.Identity (Identity (Identity))
import Data.Map.Strict (insert)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Type.Coercion (trans)
import Data.Word (Word16, Word8)
import Graphics.Gloss.Interface.Pure.Game
  ( Color,
    Display (InWindow),
    Event,
    Path,
    Picture,
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
import Streamly.Data.Array qualified as Array
import Streamly.Data.Fold qualified as Fold
import Streamly.Data.Stream qualified as Stream
import Streamly.FileSystem.File qualified as File

windowWidth :: Int
windowWidth = 1000

windowHeight :: Int
windowHeight = 500

data World = World Font Float

tick :: Float -> World -> World
tick dt (World font t) = World font (t + dt)

event :: Event -> World -> World
event e w = w

draw :: World -> Picture
draw (World font t) = let 
  p1 = drawText font $ "Seconds passed: " ++ show (floor t :: Int) 
  p2 = drawText font "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  p3 = drawText font "abcdefghijklmnopqrstuvwxy"
  p4 = drawText font "0123456789!@#$%^&*()_+{}|:\"<>?`~"
  x = negate . fromIntegral $ windowWidth `div` 2
  in pictures [translate x (-50) p1, translate x 0 p2, translate x 50 p3, translate x 100 p4]

drawText :: Font -> String -> Picture
drawText font s = pictures $ zipWith (\x c -> at x $ drawChar font c) [1 :: Int ..] s
  where
    at x = translate (fromIntegral x * (size + spacing) * 8) 0

drawChar :: Font -> Char -> Picture
drawChar font c = maybe blank drawGlyph (Map.lookup c font)

drawGlyph :: Glyph -> Picture
drawGlyph (Glyph r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15) =
  pictures $ zipWith (\y r -> at y $ drawRow r) [1 :: Int ..] rows
  where
    at y = translate 0 (fromIntegral y * (size + spacing))
    rows = [r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15]

drawRow :: Row -> Picture
drawRow (Row b0 b1 b2 b3 b4 b5 b6 b7) =
  pictures $ zipWith (\x b -> at x $ drawBit b) [1 :: Int ..] bits
  where
    bits = [b0, b1, b2, b3, b4, b5, b6, b7]
    at x = translate (fromIntegral x * (size + spacing)) 0

drawBit :: Bit -> Picture
drawBit On = color blue $ rectangleSolid size size
drawBit Off = color (dark . dark . dark . dark $ blue) $ rectangleSolid size size

size :: Float
size = 2

spacing :: Float
spacing = 1

data Bit = On | Off deriving (Enum, Show)

data Row = Row Bit Bit Bit Bit Bit Bit Bit Bit deriving (Show)

data Glyph = Glyph Row Row Row Row Row Row Row Row Row Row Row Row Row Row Row Row deriving (Show)

type Font = Map.Map Char Glyph

glyphFromRows :: [Row] -> Maybe Glyph
glyphFromRows [r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15] = Just $ Glyph r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15
glyphFromRows _ = Nothing

readFont :: (MonadIO m, MonadCatch m) => String -> m Font
readFont name =
  let readLines = Stream.foldMany (Fold.takeEndBy_ (== newLine) Fold.toList) where newLine = 0x0A
      glyphs = Stream.mapMaybe parse $ readLines (File.read name)
      parse :: [Word8] -> Maybe (Char, Glyph)
      parse (c0 : c1 : c2 : c3 : _ : rest)
        | Just g <- glyph rest = Just (toEnum . fromIntegral $ code c0 c1 c2 c3, g)
        | otherwise = Nothing
      parse _ = Nothing
      glyph hex = glyphFromRows $ go hex [] (16 :: Int)
        where
          go (c0 : c1 : rest) rows n | n /= 0 = let r = row $ byte c0 c1 in go rest (r : rows) (n - 1)
          go _ rows _ = rows
      row :: Word8 -> Row
      row c = Row (at 7) (at 6) (at 5) (at 4) (at 3) (at 2) (at 1) (at 0)
        where
          at i = if testBit c i then On else Off
      byte :: Word8 -> Word8 -> Word8
      byte hi lo = (hexDigit hi `shiftL` 4) .|. (hexDigit lo .&. 0x0F)
      code c0 c1 c2 c3 =
        let hi = fromIntegral $ byte c0 c1 :: Word16
            lo = fromIntegral $ byte c2 c3 :: Word16
         in hi `shiftL` 8 .|. (lo .&. 0x00FF)
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
      font = Fold.foldl' (\m (k, v) -> insert k v m) Map.empty
   in Stream.fold font $ Stream.take 128 glyphs

game :: Font -> IO ()
game font =
  play
    (InWindow "Seven Segment Display" (windowWidth, windowHeight) (10, 10))
    black
    20
    (World font 0.0)
    draw
    event
    tick

main :: IO ()
main = do
  font <- readFont "unifont_all-15.1.02.hex"
  game font
