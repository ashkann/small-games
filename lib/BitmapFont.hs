{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}

module BitmapFont
  ( readFont,
    drawText,
    Font,
    height,
  )
where

import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Data.Map.Strict qualified as M
import Graphics.Gloss.Interface.Pure.Game
  ( Picture,
    blank,
    pictures,
    rectangleSolid,
    translate,
  )
import Internal
import Streamly.Data.Fold qualified as Fold
import Streamly.Data.Stream qualified as Stream

readFont :: (MonadIO m, MonadCatch m) => String -> m Font
readFont fileName = Stream.fold buildFont $ Stream.take 128 glyphs
  where
    glyphs = runFile parse fileName
    buildFont = Fold.foldl' (\m (c, g) -> insertGlyph c g m) M.empty

drawText :: Font -> String -> Picture
drawText font s = pictures $ zipWith (\x c -> atChar x $ drawChar font c) [0 :: Int ..] s
  where
    atChar i = translate (fromIntegral i * 8) 0

drawChar :: Font -> Char -> Picture
drawChar font c = maybe blank drawGlyph (lookupGlyph font c)

drawGlyph :: Glyph -> Picture
drawGlyph (Glyph rows) = pictures $ zipWith f [16 :: Int, 15 ..] rows
  where
    f y (Row bits) = translate 0 (fromIntegral y) $ pictures $ zipWith bit [0 :: Int ..] bits
    bit x On = translate (fromIntegral x) 0 $ rectangleSolid 1 1
    bit _ Off = blank

width :: Float -> Float
width = (* 8)

height :: Float -> Float
height = (* 16)