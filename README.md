# A Haskell Battleship project

## About
This project is part of the Functional Programming lecture at the Unversity of Technology Hamburg during the Winter Semester 2022/2023.


## Project Goal
The end goal is the game "Battleships" on the TUHH LED wall as a oneplayer game. It has a built in emulator which can be used to play the game locally; the installation and usage instructions for both can be found below.
### How it works/What does your program do? 
The game uses a 10x10 coordinate system as the playing field. Once you start the program, the player tries to find the ships of an implemented level. After he has finished the first level the game will automatically switch to the second and then to the third. If the player has finished the third level a win screen appears. In the HUD on the left side of the playing field the player can see which ships are left and which are already sunken.
### How is it working?
Each 33000 micro seconds, we draw each time a new frame, this frame call then collects all input events and updates the pixels accordingly. Each frame is generated by first drawing the playing field and then drawing the hud elements over it.
There are two types of Input, first one is movement of cursor, which just moves the cursor and the "Enter" input, which will look at the given pixel and check what that pixel should be updated to (If there was ship hit or a miss)
Then if all ships are hit, we move onto the next level and remove all previous inputs from our stored variables. 
### What are the key (user-defined) data types and functions?
The most important data type will be "MyState" which stores all information of the given game state, it stores the cursors position, the cursors color, the current level and a list containing storing all previous inputs.
The used data type for events is "KeyStatus" which is used for formatting our given Event Key ease of use for pattern matching. It stores the status of the key (Pressed or Held), which key was pressed and for how long.

There are multiple interconnecting functions which help bring our game to life.
Our life blood is our function "toFrameList" which takes the dimensions of our screen, the pixels to draw and our current gamestate as a "MyState" data type. This will then call all other respective functions for drawing the field.
Then we have the "move" function which evaluates the given Input and transform the given "MyState" to the updated "MyState" which takes the given event and calls the according functions.
Importantly aswell are the different levels, these are stored as triple lists containing an x and y position. These represent the different levels.
Most of the rest used functions are used as helper functions, examples of these are: checking if the player has won by going through the list of hit ships (checkIfLevelFinished), checking which ships to draw at the left side of the game field (levelShipHud) and helper functions of helper functions such as (menuShipControl) which takes the list of past attacks and the current level and returns all missing ships.

## Installation
### Locally
Use SDLMain.hs in the given ZIP as the Main program, if you do not have a ready stack environment for the given LED wall, use the provided haskell-battleship-env.zip as your environment and follow the steps on the installation of SDL2 (See for reference "sdl2.pdf").
If you have a ready environment, just copy the SDLMain.hs into such, no extra outside dependencies needed.

Then you can execute it happily via the "main" function while in stack ghci.

### For the university LED wall 
Similarly to the local installation, just might need to change the given IP Address inside of SDLMain.hs (End of File, in the section "Configuration").

## Usage

### Gameplay instructions

#### Playing the game/How is the user supposed to interact?
After starting the game the player can start to try to hit the ship, by using the arrow keys &#8592; &#8593; &#8594; &#8595; to navigate to the field he intends to hit. The selection can be confirmed by hitting `Enter`. To the left of the playing field is information on which ships are already sunken. The Color doesn't change until all fields of the ship has been hit, so that the payer cannot see to which ship the part belongs which he has hitten so far.
If no ships have been hit, the colour will change to <span style="color:darkblue">Dark Blue</span>.\
If a ship has been hit, the colour of the field will change to <span style="color:red">Red</span>.\
Once a ship has been sunk, all its fields will turn <span style="color:red">Red</span> in the HUD.
After finishing the third level the game will automatically switch to the winning screen.
## Contribution
This projected is being managed and written by Henrik Bormann, Julian Koberg and Luis Platzer.
