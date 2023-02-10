import TicTacToe

main :: IO ()
main = putStrLn message
  where
    (_, winner) = game inputs emptyBoard
    message = case winner of
      Just p -> "Player " ++ show p ++ " is the winner"
      Nothing -> "No winners"
    inputs =
      [ Mark X (I1, I1),
        Mark O (I2, I2),
        Mark X (I3, I3),
        Mark O (I1, I3),
        Mark X (I3, I1),
        Mark O (I3, I2),
        Mark X (I2, I1)
      ]
