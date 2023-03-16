{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TicTacToe where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Map qualified as M
import Game qualified as G
import Prelude hiding (read)

data Index = I1 | I2 | I3 deriving (Eq, Ord)

type Pos = (Index, Index)

data Player = X | O deriving (Eq, Show)

type Cell = Maybe Player

type Board = M.Map Pos Cell

emptyBoard :: Board
emptyBoard = M.empty

data Input = Mark Player Pos

data Output = Marked Player Pos | Finished (Maybe Player)

mark :: Pos -> Player -> Board -> Board
mark pos p = M.insert pos $ Just p

cell :: Pos -> Board -> Cell
cell = M.findWithDefault Nothing

type Row = (Pos, Pos, Pos)

row :: Row -> Board -> (Cell, Cell, Cell)
row (p1, p2, p3) b = (cell p1 b, cell p2 b, cell p3 b)

rows :: [Row]
rows = [row I1, row I2, row I3, col I1, col I2, col I3, d1, d2]
  where
    row i = ((i, I1), (i, I2), (i, I3))
    col i = ((I1, i), (I2, i), (I3, i))
    d1 = ((I1, I1), (I2, I2), (I3, I3))
    d2 = ((I3, I1), (I2, I2), (I1, I3))

game :: [Input] -> Board -> (Board, Maybe Player)
game (i : is) b = case winner of
  Just _ -> (b', winner)
  Nothing -> game is b'
  where
    (b', winner) = input i b
game [] b = (b, Nothing)

input :: Input -> Board -> (Board, Maybe Player)
input (Mark p pos) b = (b', winner)
  where
    b' = mark pos p b
    winner = check rows
    check (r : rs) = case row r b' of
      (Just p', Just p2, Just p3) | p' == p2 && p' == p3 -> Just p'
      _ -> check rs
    check _ = Nothing

mkGame2 :: (Monad m, G.Read m Input, G.Write m Output) => m ()
mkGame2 = go emptyBoard
  where
    go board = do
      i <- G.read
      _ <- G.write $ marked i
      case step i board of
        (_, w@(Just _)) -> G.write (Finished w)
        (b', Nothing) -> go b'

    marked (Mark p pos) = Marked p pos

    step (Mark p pos) b = (b', winner rows)
      where
        b' = mark pos p b
        winner (r : rs) = case row r b' of
          (Just p', Just p2, Just p3) | p' == p2 && p' == p3 -> Just p'
          _ -> winner rs
        winner [] = Nothing

mkGame :: MonadIO m => TChan Input -> TChan Output -> m ()
mkGame ichan ochan = void . liftIO . forkIO $ go emptyBoard
  where
    go board = do
      i <- atomically $ readTChan ichan
      _ <- out $ marked i
      case step i board of
        (_, w@(Just _)) -> out (Finished w)
        (b', Nothing) -> go b'

    marked (Mark p pos) = Marked p pos

    out ev = atomically $ writeTChan ochan ev

    step (Mark p pos) b = (b', winner rows)
      where
        b' = mark pos p b
        winner (r : rs) = case row r b' of
          (Just p', Just p2, Just p3) | p' == p2 && p' == p3 -> Just p'
          _ -> winner rs
        winner [] = Nothing