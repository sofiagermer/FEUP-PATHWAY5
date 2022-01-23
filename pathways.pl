:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(between)).
:- use_module(library(system)).

:- include('board.pl').       /* Game displaying functions */
:- include('input.pl').       /* Game displaying functions */
:- include('menu.pl').         /* Helper functions */
:- include('utils.pl').         /* Helper functions */
:- include('logic.pl').         /* Helper functions */

play :- game.

nextPlayer(1,2).
nextPlayer(2,1).

game:-
    displayGameTitle,
    sleep(1),
    menu(Board,GameMode),
    gameLoop(GameMode,Board,1).

gameLoop(1,Board,Player) :-
    gameOver(Board,Player),
    displayGameOver,
    nextPlayer(Player,NewPlayer),
    displayWinningPlayer(NewPlayer).

gameLoop(1,Board,Player) :-
    \+ gameOver(Board,Player)->(
    displayBoard6(Board,Player),
    nextMove(Board,Player,TempBoard),
    nextPlayer(Player,NewPlayer),
    sleep(1),
    randomMove(TempBoard,NewPlayer,Move),
    (emptyList(Move)->gameLoop(1,NewBoard,NewPlayer);
    getLineElement(0,Move,Line),
    getLineElement(1,Move,Column),
    replaceBoardElement(TempBoard,Line,Column,NewPlayer,NewBoard),
    gameLoop(1,NewBoard,Player))).

gameLoop(2,Board,Player) :-
    gameOver(Board,Player),
    displayGameOver,
    nextPlayer(Player,NewPlayer),
    displayWinningPlayer(NewPlayer).

gameLoop(2,Board,Player) :-
    \+ gameOver(Board,Player)->(
    displayBoard6(Board,Player),
    nextMove(Board,Player,NewBoard),
    nextPlayer(Player,NewPlayer),
    gameLoop(2,NewBoard,NewPlayer)).
    
gameLoop(3,Board,Player) :-
    gameOver(Board,Player),
    displayGameOver,
    nextPlayer(Player,NewPlayer),
    displayWinningPlayer(NewPlayer).

gameLoop(3,Board,Player) :-
    \+ gameOver(Board,Player)->(
    displayBoard6(Board,Player),
    sleep(1),
    randomMove(Board,Player,Move1),
    getLineElement(0,Move1,Line1),
    getLineElement(1,Move1,Column1),
    replaceBoardElement(Board,Line1,Column1,Player,TempBoard),
    displayBoard6(TempBoard,Player),
    nextPlayer(Player,NewPlayer),
    sleep(1),
    randomMove(TempBoard,NewPlayer,Move2),
    (emptyList(Move2)->gameLoop(3,NewBoard,NewPlayer);
    getLineElement(0,Move2,Line2),
    getLineElement(1,Move2,Column2),
    replaceBoardElement(TempBoard,Line2,Column2,NewPlayer,NewBoard),
    gameLoop(3,NewBoard,Player))).



initial([
    [0,0,0,0,0,0],
    [0,0,0,0,0,0],
    [0,0,0,0,0,0],
    [0,0,0,0,0,0],
    [0,0,0,0,0,0],
    [0,0,0,0,0,0]
    ]).

test([
    [1,0,1,2,0,1],
    [1,0,0,2,2,2],
    [2,0,0,0,2,0],
    [0,1,1,0,0,1],
    [0,2,0,1,1,0],
    [2,2,0,0,0,0]
    ]).

full([
    [1,1,1,2,1,1],
    [1,1,1,2,2,2],
    [2,1,1,1,2,1],
    [1,1,1,1,1,1],
    [1,2,1,1,2,1],
    [2,2,1,2,0,1]
    ]).


gameOver(Board,Player):-
    length(Board,N),
    M is N*N,
    validMoves(Board,Player,M,LL,F),!,
    emptyList(F).
