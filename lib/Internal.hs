{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Internal
  ( Font,
    Bit (..),
    Glyph (..),
    insertGlyph,
    lookupGlyph,
    Row (..),
    runFile,
    parse,
  )
where

import Control.Monad (replicateM)
import Control.Monad.Catch (Exception, MonadCatch, MonadThrow (throwM))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State.Lazy qualified as S
import Data.Bits (Bits (testBit), shiftL, (.|.))
import Data.Char (digitToInt, isHexDigit)
import Data.Map.Strict qualified as M
import Data.Word (Word16, Word8)
import GHC.Base (Applicative (liftA2), (<|>))
import GHC.Char (chr)
import Streamly.Data.Fold qualified as Fold
import Streamly.Data.Stream qualified as Stream
import Streamly.FileSystem.File qualified as File

data Bit = On | Off deriving (Enum, Show)

data Row = Row [Bit]

data Glyph = Glyph [Row]

mkRow :: Word8 -> Row
mkRow c = Row [if testBit c i then On else Off | i <- [7, 6 .. 0]]

rowsPerGlyph :: Int
rowsPerGlyph = 16

mkGlyph :: [Row] -> Glyph
mkGlyph rows | length rows == rowsPerGlyph = Glyph rows
mkGlyph _ = error "Glyph must be 16 rows tall"

type Parse m = S.StateT String m

newtype Error = Error String deriving (Show)

instance Exception Error

err :: (MonadThrow m) => String -> m a
err = throwM . Error

runFile :: (MonadIO m, MonadCatch m) => Parse m a -> String -> Stream.Stream m a
runFile p = run p . fmap (chr . fromIntegral) . File.read

run :: (Monad m) => Parse m a -> Stream.Stream m Char -> Stream.Stream m a
run p = Stream.mapM (S.evalStateT p) . readLines
  where
    readLines = Stream.foldMany (Fold.takeEndBy_ (== '\n') Fold.toList)

parse :: (MonadThrow m) => Parse m (Char, Glyph)
parse = do
  c <- codePoint
  _ <- next >>= match ':'
  g <- glyph
  return (c, g)
  where
    match c ch = if ch == c then return ch else err $ "Expected `" ++ [c] ++ "` but got `" ++ [ch] ++ "`"

glyph :: (MonadThrow m) => Parse m Glyph
glyph = mkGlyph <$> replicateM rowsPerGlyph (mkRow <$> byte2)

codePoint :: (MonadThrow m) => Parse m Char
codePoint = toEnum . fromIntegral <$> word2

next :: (MonadThrow m) => Parse m Char
next = S.get >>= f
  where
    f (c : rest)
      | check c = S.put rest >> return c
      | otherwise = err $ "Unexpected character: `" ++ [c] ++ "`"
    f [] = err "End of input"
    check c = isHexDigit c || c == ':'

byte2 :: (MonadThrow m) => Parse m Word8
byte2 = liftA2 byte next next

word2 :: (MonadThrow m) => Parse m Word16
word2 = liftA2 word byte2 byte2

byte :: Char -> Char -> Word8
byte hi lo = c hi `shiftL` 4 .|. c lo
  where
    c = fromIntegral . digitToInt

word :: Word8 -> Word8 -> Word16
word hi lo = fromIntegral hi `shiftL` 8 .|. fromIntegral lo

type Font = M.Map Char Glyph

lookupGlyph :: Font -> Char -> Maybe Glyph
lookupGlyph font c = M.lookup c font <|> replacement

insertGlyph :: Char -> Glyph -> Font -> Font
insertGlyph = M.insert

replacement :: (MonadThrow m) => m Glyph
replacement = runStr glyph "0000007E665A5A7A76767E76767E0000"

runStr :: (MonadThrow m) => Parse m a -> String -> m a
runStr p cs = do
  maybeA <- Stream.fold Fold.one (run p $ Stream.fromList cs)
  maybe (err "Failed to parse glyph") return maybeA
