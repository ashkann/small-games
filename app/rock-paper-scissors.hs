import RockPaperScissors

main :: IO ()
main = putStrLn message
  where
    (_, winner) = game inputs emptyGame
    message = case winner of
      Just p -> "Winner is " ++ show p
      _ -> "Game is a draw"

    inputs =
      [ Input Rock Rock,
        Input Paper Scissors,
        Input Scissors Rock,
        Input Paper Rock,
        Input Rock Paper
      ]
