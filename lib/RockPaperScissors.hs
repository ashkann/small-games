module RockPaperScissors where

import qualified Data.Map as M

data Hand = Rock | Paper | Scissors deriving (Eq, Ord)

beats :: Hand -> Hand -> Ordering
beats Rock Scissors = GT
beats Scissors Rock = LT
beats x y = compare x y

data Score = S0 | S1 | S2 | S3 deriving (Eq, Enum)

data Player = P1 | P2 deriving (Eq, Ord, Show)

data Input = Input Hand Hand

type Game = M.Map Player Score

score :: Player -> Game -> Score
score = M.findWithDefault S0

scored :: Player -> Game -> Game
scored = M.alter $ (\x -> case x of 
  Just s -> Just $ succ s
  Nothing -> Just S1
  )

emptyGame :: Game
emptyGame = M.empty

input :: Input -> Game -> (Game, Maybe Player)
input (Input x y) g = case w of
  Just p ->
    let g' = scored p g
        winner = case (score P1 g', score P2 g') of
          (S3, _) -> Just P1
          (_, S3) -> Just P2
          _ -> Nothing
     in (g', winner)
  Nothing -> (g, Nothing)
  where
    w = case beats x y of
      GT -> Just P1
      LT -> Just P2
      EQ -> Nothing

game :: [Input] -> Game -> (Game, Maybe Player)
game (i : is) g = case winner of
  Just _ -> (g', winner)
  Nothing -> game is g'
  where
    (g', winner) = input i g
game [] g = (g, Nothing)