# A Haskell Battleship project

## About
This project is part of the Functional Programming lecture at the Unversity of Technology Hamburg during the Winter Semester 2022/2023.


## Project Goal
The end goal is the game "Battleships" on the TUHH LED wall. It has a built in emulator which can be used to play the game locally; the installation and usage instructions for both can be found below.
### How it works/What does your program do? 
The game uses two 10x10 coordinate systems as the playing fields. Once you start the program, the first player starts to place his ships. Afterwards the second player does the same on his field. Afterwards the players change sides and every player searches the ships, the other one has hidden on the field. The first player who destroys all ships of his enemy wins the game. Because the ships can be placed everywhere in the field, even several on the same position, the players are responsible for the correct placement. If a ship is moved near to the border and some parts are outside of the border, the game will stop with an error if the player tries to place the ship.
### How is it working?
Each 33000 micro seconds, we draw each time a new frame, this frame call then collects all input events and updates the pixels accordingly.
There are three types of Input, first one is movement of cursor, which just moves the cursor, second one is the "Enter" input, which will look at the given pixel and check what that pixel should be updated to (If there was ship hit or a miss) and which is also used to place a ship. And the third one is "Space" which is used to rotate the ships during the placement, so that the ships can be placed horizontally and vertically.
Then if all ships of one player are hit, we change to a winning screen on which is displayed which player has won the game.
### What are the key (user-defined) data types and functions?
The most important data type will be "MyState" which stores all information of the given game state, it stores the cursors position, the cursors color, the cursorMode, the field to draw, two list containing all previous inputs, one list for each player, two list containing the ships of each player, the rotation and the length of the ship which is placed in the moment.
The used data type for events is "KeyStatus" which is used for formatting our given Event Key ease of use for pattern matching. It stores the status of the key (Pressed or Held), which key was pressed and for how long.

There are multiple interconnecting functions which help bring our game to life.
Our life blood is our function "toFrameList" which takes the dimensions of our screen, the pixels to draw and our current gamestate as a "MyState" data type. This will then call all other respective functions for drawing the field.
Then we have the "move" function which evaluates the given Input and transform the given "MyState" to the updated "MyState" which takes the given event and calls the according functions.
Most of the rest used functions are used as helper functions, examples of these are: checking if the player has won by going through the list of hit ships (checkIfLevelFinished), checking which ships to draw at the left side of the game field (levelShipHud) and helper functions of helper functions such as (menuShipControl) which takes the list of past attacks and the current level and returns all missing ships.

## Installation
### Locally
Use SDLTwoPlayer.hs in the given ZIP as the Main program, if you do not have a ready stack environment for the given LED wall, use the provided haskell-battleship-env.zip as your environment and follow the steps on the installation of SDL2 (See for reference "sdl2.pdf").
If you have a ready environment, just copy the SDLTwoPlayer.hs into such, no extra outside dependencies needed.

Then you can execute it happily via the "main" function while in stack ghci.

### For the university LED wall 
Similarly to the local installation, just might need to change the given IP Address inside of SDLTwoPlayer.hs (End of File, in the section "Configuration").

## Usage

### Gameplay instructions

#### Playing the game/How is the user supposed to interact?
After starting the game the player can move over the field to place their ships, by using the arrow keys &#8592; &#8593; &#8594; &#8595;, and change the rotation of the ships by pressing "Space". The selection can be confirmed by hitting `Enter`. After both players has hidden their ships they start to attack the enemies field. If they hit a ship they can attack again otherwise its the other players turn. If one player has hitten all of his enemies ships, he wins the game.\
If no ships have been hit, the colour will change to <span style="color:darkblue">Dark Blue</span>.\
If a ship has been hit, the colour of the field will change to <span style="color:red">Red</span>.\
After one player has won, the game will automatically switch to the winning screen.
## Contribution
This projected is being managed and written by Henrik Bormann, Julian Koberg and Luis Platzer.
