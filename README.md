# Pathways Game - Group 5

## Group 5 - Members

|----------------------------------------|
| Name             | E-Mail              |
| ---------------- |-------------------- |
| Sofia Germer     | up201907461@up.pt   |
| Miguel Lopes     | up201704590@up.pt   |

## Pathways

### Setup and Install

This program was developed and tested using SICStus Prolog. Instruction on how to install SICStus can be found [here](https://sicstus.sics.se/download4.html).

To load the game consult the pathways.pl file. Finally to start the game use the **play/0** predicate:

```pl
play.
```

### Description and Rules

Pathway is a two player game played on a 6x6 initially empty checkerboard. The two players, Player 1 and Player 2, take turns placing their own checkers on the board, one checker per turn, starting with Player 1. Players are not allowed to pass. 

There are 2 types of connections:

- FRIENDLY CONNECTION is an adjacency between checkers from the same player.
- ENEMY CONNECTION is an adjacency between different players' checkers.

There are 2 types of placements a player can make:

- The player can place a checker to form no connections (neither friendly nor enemy). 
- The player can place a checker to form exactly one friendly connection and zero or more enemy connections.

The first player not to have an available placement wins.

Mark Steere designed Pathway in March, 2021 - Full game description [here](http://www.marksteeregames.com/Pathway_rules.pdf).

### Game Logic

#### Internal Representation

The board represents the state of the game at all times, and is used to determine and validate moves.

To implement the board, we used a list of lists (6x6). Each cell contains either:

- **a 0** to represent an empty position
- **a 1** to represent positions occupied by Player 1's pieces
- **a 2** to represent positions occupied by Player 2's pieces

The board is initialized with the predicate **initial/1**:

```pl
initial([
    [0,0,0,0,0,0],
    [0,0,0,0,0,0],
    [0,0,0,0,0,0],
    [0,0,0,0,0,0],
    [0,0,0,0,0,0],
    [0,0,0,0,0,0]
    ]).
```
The following screenshots are examples of the board at three different stages of the game:

[]