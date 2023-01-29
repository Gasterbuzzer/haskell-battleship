# A Haskell Battleship project

## About
This project is part of the Functional Programming lecture at the Unversity of Technology Hamburg during the Winter Semester 2022/2023.


## Project Goal
The end goal is the game "Battleships" on the TUHH LED wall as a oneplayer game. It has a built in emulator which can be used to play the game locally; the installation and usage instructions for both can be found below.
### How it works
The game uses a 10x10 coordinate system as the playing field. Once you start the program, the player tries to find the ships of an implemented level. After he has finished the first level the game will automatically switch to the second and then to the third. If the player has finished the third level a win screen appears. In the HUD on the right side of the playing field the player can see which ships are left and which are already sunken.

## Installation
### Locally

### For the university LED wall 

## Usage

### Gameplay instructions

#### Playing the game
After starting the game the player can start to try to hit the ship, by using the arrow keys &#8592; &#8593; &#8594; &#8595; to navigate to the field he intends to hit. The selection can be confirmed by hitting `Enter`. To the right of the playing field is information on which ships are already sunken. The Color doesn't change until all fields of the ship has been hit, so that the payer cannot see to which ship the part belongs which he has hitten so far.
If no ships have been hit, the colour will change to <span style="color:darkblue">Dark Blue</span>.\
If a ship has been hit, the colour of the field will change to <span style="color:red">Red</span>.\
Once a ship has been sunk, all its fields will turn <span style="color:red">Red</span> in the HUD.
After finishing the third level the game will automatically switch to the winning screen.
## Contribution
This projected is being managed and written by Henrik Bormann, Julian Koberg and Luis Platzer.
