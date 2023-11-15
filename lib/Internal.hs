{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Internal where

import Control.Monad.Catch (MonadCatch, MonadThrow (throwM))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State.Lazy qualified as S
import Data.Bits (shiftL, (.|.))
import Data.Char (digitToInt, isHexDigit)
import Data.Word (Word16, Word8)
import GHC.Base (Applicative (liftA2))
import GHC.Char (chr)
import Streamly.Data.Fold qualified as Fold
import Streamly.Data.Stream qualified as Stream
import Streamly.FileSystem.File qualified as File

type Parse m = S.StateT String m

runFile :: (MonadIO m, MonadCatch m) => Parse m a -> String -> Stream.Stream m a
runFile p = run p . fmap (chr . fromIntegral) . File.read

run :: (Monad m) => Parse m a -> Stream.Stream m Char -> Stream.Stream m a
run p = Stream.mapM (S.evalStateT p) . readLines
  where
    readLines = Stream.foldMany (Fold.takeEndBy_ (== '\n') Fold.toList)

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

err :: (MonadThrow m) => String -> m a
err s = throwM $ userError s

byte :: Char -> Char -> Word8
byte hi lo = c hi `shiftL` 4 .|. c lo
  where
    c = fromIntegral . digitToInt

word :: Word8 -> Word8 -> Word16
word hi lo = fromIntegral hi `shiftL` 8 .|. fromIntegral lo