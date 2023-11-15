{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}

module BitmapFont
  ( sqaureDotStyle,
    readFont,
    drawText,
    Font,
    Style (height, width),
  )
where

import Control.Monad (replicateM)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State.Lazy qualified as S
import Data.Bits (testBit)
import Data.Map.Strict qualified as M
import Data.Word (Word8)
import GHC.Base ((<|>))
import GHC.Char (chr)
import Graphics.Gloss.Interface.Pure.Game
  ( Color,
    Picture,
    blank,
    color,
    pictures,
    rectangleSolid,
    translate,
  )
import Streamly.Data.Fold qualified as Fold
import Streamly.Data.Stream qualified as Stream
import Streamly.FileSystem.File qualified as File
import Internal

drawText :: Style -> Font -> String -> Picture
drawText style font s = pictures $ zipWith (\x c -> atChar style x $ drawChar style font c) [0 :: Int ..] s

drawChar :: Style -> Font -> Char -> Picture
drawChar style font c = case M.lookup c font <|> replacement of
  Just g -> drawGlyph style g
  Nothing -> blank

replacement :: (MonadThrow m) => m Glyph
replacement = runStr glyph "0000007E665A5A7A76767E76767E0000"

drawGlyph :: Style -> Glyph -> Picture
drawGlyph style (Glyph r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15) =
  pictures $ zipWith (\y r -> atY style y $ drawRow style r) [1 :: Int ..] rows
  where
    rows = [r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15]

drawRow :: Style -> Row -> Picture
drawRow style (Row b0 b1 b2 b3 b4 b5 b6 b7) =
  pictures $ zipWith drw [1 :: Int ..] [b0, b1, b2, b3, b4, b5, b6, b7]
  where
    drw x On = atX style x $ on style
    drw _ Off = blank

data Style = Style
  { on :: Picture,
    atX :: Int -> Picture -> Picture,
    atY :: Int -> Picture -> Picture,
    atChar :: Int -> Picture -> Picture,
    width :: Float,
    height :: Float
  }

sqaureDotStyle :: Color -> Float -> Float -> Style
sqaureDotStyle c size space =
  let dot = rectangleSolid size size
      s i = fromIntegral i * (size + space)
      dotsPerRow = 8
      rowsPerGlyph = 16
      w = s dotsPerRow
   in Style
        { on = color c dot,
          atX = \x -> translate (s x) 0,
          atY = translate 0 . s,
          atChar = \i -> translate (fromIntegral i * w) 0,
          width = w,
          height = s rowsPerGlyph
        }

data Bit = On | Off deriving (Enum, Show)

data Row = Row Bit Bit Bit Bit Bit Bit Bit Bit deriving (Show)

mkRow :: Word8 -> Row
mkRow c = Row (at 7) (at 6) (at 5) (at 4) (at 3) (at 2) (at 1) (at 0)
  where
    at i = if testBit c i then On else Off

row0 :: Row
row0 = Row Off Off Off Off Off Off Off Off

row1 :: Row
row1 = Row On On On On On On On On

data Glyph = Glyph Row Row Row Row Row Row Row Row Row Row Row Row Row Row Row Row deriving (Show)

mkGlyph :: [Row] -> Maybe Glyph
mkGlyph [r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15] = Just $ Glyph r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15
mkGlyph _ = Nothing

type Font = M.Map Char Glyph

readFont :: (MonadIO m, MonadCatch m) => String -> m Font
readFont fileName = Stream.fold addToFont $ Stream.take 128 glyphs
  where
    glyphs = runFile parse fileName
    addToFont = Fold.foldl' (\m (c, g) -> M.insert c g m) M.empty

runStr :: (MonadThrow m) => Parse m a -> String -> m a
runStr p cs = do
  maybeA <- Stream.fold Fold.one (run p $ Stream.fromList cs)
  maybe (err "Failed to parse glyph") return maybeA

parse :: (MonadThrow m) => Parse m (Char, Glyph)
parse = do
  c <- codePoint
  _ <- next >>= match ':'
  g <- glyph
  return (c, g)
  where
    match c ch = if ch == c then return ch else err $ "Expected `" ++ [c] ++ "` but got `" ++ [ch] ++ "`"

glyph :: (MonadThrow m) => Parse m Glyph
glyph = rows >>= fromMaybe . mkGlyph . reverse
  where
    rows = replicateM 16 $ mkRow <$> byte2
    fromMaybe (Just a) = return a
    fromMaybe Nothing = err "Failed to parse glyph"

codePoint :: (MonadThrow m) => Parse m Char
codePoint = toEnum . fromIntegral <$> word2    