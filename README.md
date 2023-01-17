# A Haskell Battleship project

## About
This project is part of the Functional Programming lecture at the Unversity of Technology Hamburg during the Winter Semester 2022/2023.


## Project Goal
The end goal is the game "Battleships" on the TUHH LED wall. It has a built in emulator which can be used to play the game locally; the installation and usage instructions for both can be found below.
### How it works
The game uses a XxX coordinate system as the playing field. Once you start the program, the players can place their ships. Afterwards, it switches between blank playing fields, with the information about player/ship status on the right. The goal of the game is to sink all enemy ships.

## Installation
### Locally

### For the university LED wall 

## Usage

### Gameplay instructions
The game starts with placement for Player 1; once he is done, Player 2 can place their ships.
#### Placing your ships
Ships can be placed using the arrow keys &#8592; &#8593; &#8594; &#8595;,  `Space` to turn ships clockwise and `Enter` to lock in their placement.

#### Playing the game
Once all ships have been placed, the game starts. Players can now take turns attempting to hit the other players ships, again by using the arrow keys &#8592; &#8593; &#8594; &#8595; to navigate to the field they intend to hit. The selection can be confirmed by hitting `Enter`. To the right of the playing field is information on which ships will still need to be placed.
If no ships have been hit, the colour will change to <span style="color:white">White</span>.\
If a ship has been hit, the colour of the field will change to <span style="color:orange">Orange</span>.\
Once a ship has been sunk, all its fields will turn <span style="color:red">Red</span>. Once a ship has been sunk, it will also turn <span style="color:red">Red</span> in the game menu sidebar.
## Contribution
This projected is being managed and written by Henrik Bormann, Julian Koberg and Luis Platzer.