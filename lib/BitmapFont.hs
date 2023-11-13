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
import Control.Monad.Catch (MonadCatch, MonadThrow (throwM))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State.Lazy qualified as S
import Control.Monad.Trans (MonadTrans (lift))
import Data.Bits (shiftL, testBit, (.|.))
import Data.Char (digitToInt)
import Data.List (uncons)
import Data.Map.Strict qualified as M
import Data.Word (Word16, Word8)
import GHC.Base (Applicative (liftA2), (<|>))
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

drawText :: Style -> Font -> String -> Picture
drawText style font s = pictures $ zipWith (\x c -> atChar style x $ drawChar style font c) [0 :: Int ..] s

drawChar :: Style -> Font -> Char -> Picture
drawChar style font c = case M.lookup c font <|> glyph2 replacement of
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
  pictures $ zipWith drw [1 :: Int ..] bits
  where
    drw x On = atX style x $ on style
    drw _ Off = blank
    bits = [b0, b1, b2, b3, b4, b5, b6, b7]

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
          atY = \y -> translate 0 (s y),
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

byte :: Char -> Char -> Word8
byte hi lo = (c hi `shiftL` 4) .|. c lo
  where
    c = fromIntegral . digitToInt

word :: Word8 -> Word8 -> Word16
word hi lo = (fromIntegral hi `shiftL` 8) .|. fromIntegral lo

type Parse = S.StateT [Char] Maybe

readFont :: (MonadIO m, MonadCatch m) => String -> m Font
readFont fileName = Stream.fold addToFont $ Stream.take 128 glyphs
  where
    glyphs = Stream.mapM (run parse) $ Stream.foldMany readLine file
    readLine = Fold.takeEndBy_ (== '\n') Fold.toList
    file = chr . fromIntegral <$> File.read fileName
    addToFont = Fold.foldMap $ uncurry M.singleton

run :: (MonadThrow m) => Parse a -> [Char] -> m a
run p = nothingAsErr . S.evalStateT p
  where
    nothingAsErr = maybe (throwM $ userError "Parse error") return

parse :: Parse (Char, Glyph)
parse = do
  c <- codePoint
  _ <- match ':'
  g <- glyph
  return (c, g)
  where
    match c = next >>= (\ch -> lift $ if ch == c then Just () else Nothing)

next :: Parse Char
next = S.StateT uncons

byte2 :: Parse Word8
byte2 = liftA2 byte next next

word2 :: Parse Word16
word2 = liftA2 word byte2 byte2

codePoint :: Parse Char
codePoint = toEnum . fromIntegral <$> word2

glyph :: Parse Glyph
glyph = let rows = replicateM 16 $ mkRow <$> byte2 in (rows >>= lift . mkGlyph . reverse)