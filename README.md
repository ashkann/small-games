# Introduction
A bundle of small games done in Haskell, offered via a game server of sorts

At the moment there are two games: Tic-tac-to and Rock-paper-scissor. Each game needs two players to
start and others can join to watch a game in progress.

Players can create a game (which will create a room) or join a game (room). The second player that jois a game (room) causes the game to start and whoever joins later is watching the game

# Client
## brick (tui)
First I tried a tui client using `brick`. This quickly proved to be very limiting so I shifted to graphical clients.
## gloss (vector)
Vector graphics is a perfect fit for a tic-tac-to client but `gloss` didn't work well with `DearImGui` and eventualy I had to abandon `gloss` for a solution with better integration with `DearImGui`.
## SDL with OpenGL rendering
`SDL2` would have been even a more perfect fit since it handles sound and inut as well and is cross platform on top. Unfortunatly its Haskell bindings are limited to OpenGL 2.1 surfaces for graphics.
## Why not SDL without rendering + vanila OpenG ?
OpenGL has a very uncertain future on Mac. Besides, if I'm going to let go of an engine in favour of a lower level solution then Vulkan wins over OpenGL.
## SDL with Vulkan rendering
`getRenderDriverInfo` returns `metals`, `software` and a bunch of OpenGLs so SDL doesn't have Vulkan rendering. It has Vulkan context for the window creation so you can do your own Vulkan.
## SDL without rendering + vanila Vulkan
After trying literally everything else, vanila Vulkan seems to be the way to go for the graphics (rendering) part. SDL handles everything else (input, sound etc).

# Session
A game happens in a Session. Session starts when a player joins. Session continues when the second player joins and game starts. Session is still on going during the game and session ends when game finishes. Session is identified by `GameId`