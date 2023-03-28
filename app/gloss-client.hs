{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

import Data.Map qualified as M
-- import Debug.Trace (trace)
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import TicTacToe qualified as T3

data World = World
  { pointer :: Point,
    board :: T3.Board,
    player :: Maybe T3.Player
  }

main :: IO ()
main = play d backgroundColor 1 emptyWorld draw event (const id)
  where
    emptyWorld = World {pointer = (0, 0), board = T3.emptyBoard, player = Just T3.O}
    d = InWindow "Tic-Tac-Toe" (size, size) (100, 100)
    size = 500 :: Int
    backgroundColor = makeColor 0 0 0 0
    gridColor = makeColor 1 1 1 1
    pointerColor = makeColor 1 1 1 0.2
    xColor = makeColor 1 0 0 1
    oColor = makeColor 0 0 1 1
    margin = 20
    padding = 10
    cw = fromIntegral $ (size - 2 * margin) `div` 6
    end = fromIntegral $ size `div` 2 - margin
    cw2 = fromIntegral $ (size - 2 * margin) `div` 3
    hline = color gridColor $ line [(-end, 0), (end, 0)]
    vline = color gridColor $ line [(0, -end), (0, end)]
    cell r c = row . col
      where
        row = translate 0 $ move r
        col = translate (move c) 0
    move = \case
      T3.I1 -> -cw2
      T3.I2 -> 0
      T3.I3 -> cw2
    -- Event handling
    event ev w
      | Just (x, y) <- clicked, World {player = Just p, board = b} <- w = w {board = T3.mark (toPos x y) p b}
      | pressedX = w {player = Just T3.X}
      | pressedO = w {player = Just T3.O}
      | (EventMotion point) <- ev = w {pointer = point}
      | otherwise = w
      where
        pressedX
          | EventKey (Char 'x') Down (Modifiers Up Up Up) _ <- ev = True
          | otherwise = False
        pressedO
          | EventKey (Char 'o') Down (Modifiers Up Up Up) _ <- ev = True
          | otherwise = False
        clicked
          | EventKey (MouseButton LeftButton) Down (Modifiers Up Up Up) (x, y) <- ev = Just (x, y)
          | otherwise = Nothing
    -- Drawing the game
    draw World {pointer = (x, y), board = board} = Pictures [drawGrid, drawBoard, drawPointer]
      where
        drawGrid =
          Pictures
            [ translate 0 (-cw) hline,
              translate 0 cw hline,
              translate (-cw) 0 vline,
              translate cw 0 vline
            ]
        drawPointer = color pointerColor $ uncurry cell (toPos x y) $ rectangleSolid 100 100
        drawBoard = Pictures $ map drawCell $ M.toList board
        drawCell ((r, c), p) = cell r c $ maybe Blank drawPlayer p
        drawPlayer = \case
          T3.X -> color xColor drawX
          T3.O -> color oColor drawO
          where
            drawX = let a = cw - padding in Pictures [line [(-a, a), (a, -a)], line [(-a, -a), (a, a)]]
            drawO = circle $ cw - padding
    toPos x y = (toIndex y, toIndex x)
      where
        toIndex cord
          | cord <= -cw = T3.I1
          | cord <= cw = T3.I2
          | otherwise = T3.I3
