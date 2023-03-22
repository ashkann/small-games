{-# LANGUAGE LambdaCase #-}

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

data I = I1 | I2 | I3

newtype World = World {pointer :: Point}

main :: IO ()
main = play d backgroundColor 1 (World {pointer = (0, 0)}) draw event (const id)
  where
    d = InWindow "Tic-Tac-Toe" (size, size) (100, 100)
    size = 500 :: Int
    backgroundColor = makeColor 0 0 0 0
    board =
      Pictures
        [ translate 0 (-cw) hline,
          translate 0 cw hline,
          translate (-cw) 0 vline,
          translate cw 0 vline,
          drawX I1 I2,
          drawO I2 I2
        ]
    c1 = makeColor 1 1 1 1
    margin = 20
    padding = 10
    cw = fromIntegral $ (size - 2 * margin) `div` 6
    end = fromIntegral $ size `div` 2 - margin
    hline = color c1 $ line [(-end, 0), (end, 0)]
    vline = color c1 $ line [(0, -end), (0, end)]
    x = let a = cw - padding in Pictures [line [(-a, a), (a, -a)], line [(-a, -a), (a, a)]]
    o = circle $ cw - padding
    xColor = makeColor 1 0 0 1
    oColor = makeColor 0 0 1 1
    drawX r c = cell r c $ color xColor x
    drawO r c = cell r c $ color oColor o
    cell r c = row . col
      where
        row = translate 0 $ move r
        col = translate (move c) 0
    move =
      let cw2 = fromIntegral $ (size - 2 * margin) `div` 3
       in \case
            I1 -> -cw2
            I2 -> 0
            I3 -> cw2
    event (EventKey (MouseButton LeftButton) Down (Modifiers Up Up Up) (x, y)) w = w
    event (EventMotion (x, y)) w = World {pointer = (x, y)}
    event _ w = w
    draw World {pointer = (x, y)} = color c1 $ translate x y $ circle 5
