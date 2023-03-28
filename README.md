# Introduction
A bundle of small games done in Haskell, offered via a game server of sorts

At the moment there are two games: Tic-tac-to and Rock-paper-scissor. Each game needs two players to
start and others can join to watch a game in progress.

Players can create a game (which will create a room) or join a game (room). The second player that jois a game (room) causes the game to start and whoever joins later is watching the game

# Client
## TUI (brick)
First I tried a tui client using `brick`. This quickly proved to be very limiting so I shifted to graphical clients.
## Vector (gloss)
Vector graphics is a perfect fit for a tic-tac-to client but `gloss` didn't work well with `DearImGui` and eventualy I had to abandon `gloss` for a solution with better integration with `DearImGui`
## 2D (SDL)
`SDL2` would have been even a more perfect fit since it handles sound and inut as well and is cross platform on top. Unfortunatly its Haskell bindings are limited to OpenGL 2.1 surfaces for graphics.
## Why not OpenG ?
OpenGL has a very uncertain future on Mac. Besides, if I'm going to let go of an engine in favour of a lower solution then why not go with Vulkan ?
## Vulkan
Actually has a very descent binding in Haskell. Problem is its very low level and verbose. It's essentailly all shaders (no primitives like Lines, Points etc)