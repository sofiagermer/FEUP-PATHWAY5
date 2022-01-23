:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(between)).
:- use_module(library(system)).

:- include('views.pl').       /* Game displaying functions */
:- include('menu.pl').         /* Menu functions */
:- include('utils.pl').         /* Generic auxiliar functions */
:- include('logic.pl').         /* Game logic and validation functions */

%function that is called to play the game
play :- game.

%game title is displayed and a sleep(1) is used to improve the user's experience
%the menu where the configurations of the game will be chosen is displayed
%game loop is called
game:-
    display_game_title,
    sleep(1),
    menu(Board,GameMode),
    game_loop(GameMode,Board,1).

%function that is responsable for the game logic
game_loop(_,Board,Player) :-
    game_over(Board,Player),
    display_game_over,
    next_player(Player,NewPlayer),
    display_winning_player(NewPlayer).

game_loop(1,Board,Player) :-
    \+ game_over(Board,Player)->(
    display_game(Board,Player),
    move(Board,Player,TempBoard),
    next_player(Player,NewPlayer),
    display_game(TempBoard,NewPlayer),
    sleep(1),
    random_move(TempBoard,NewPlayer,Move),
    (empty_list(Move)->game_loop(1,NewBoard,NewPlayer);
    nth0(0,Move,Line),
    nth0(1,Move,Column),
    replace_board_element(TempBoard,Line,Column,NewPlayer,NewBoard),
    game_loop(1,NewBoard,Player))).


game_loop(2,Board,Player) :-
    \+ game_over(Board,Player)->(
    display_game(Board,Player),
    move(Board,Player,TempBoard),
    next_player(Player,NewPlayer),
    display_game(TempBoard,NewPlayer),
    sleep(1),
    smart_move(TempBoard,NewPlayer,Move),
    (empty_list(Move)->game_loop(2,NewBoard,NewPlayer);
    nth0(0,Move,Line),
    nth0(1,Move,Column),
    replace_board_element(TempBoard,Line,Column,NewPlayer,NewBoard),
    game_loop(2,NewBoard,Player))).



game_loop(3,Board,Player) :-
    \+ game_over(Board,Player)->(
    display_game(Board,Player),
    move(Board,Player,NewBoard),
    next_player(Player,NewPlayer),
    game_loop(3,NewBoard,NewPlayer)).


game_loop(4,Board,Player) :-
    \+ game_over(Board,Player)->(
    display_game(Board,Player),
    sleep(1),
    random_move(Board,Player,Move1),
    nth0(0,Move1,Line1),
    nth0(1,Move1,Column1),
    replace_board_element(Board,Line1,Column1,Player,TempBoard),
    next_player(Player,NewPlayer),
    display_game(TempBoard,NewPlayer),
    sleep(1),
    random_move(TempBoard,NewPlayer,Move2),
    (empty_list(Move2)->game_loop(4,NewBoard,NewPlayer);
    nth0(0,Move2,Line2),
    nth0(1,Move2,Column2),
    replace_board_element(TempBoard,Line2,Column2,NewPlayer,NewBoard),
    game_loop(4,NewBoard,Player))).


game_loop(5,Board,Player) :-
    \+ game_over(Board,Player)->(
    display_game(Board,Player),
    sleep(1),
    smart_move(Board,Player,Move1),
    nth0(0,Move1,Line1),
    nth0(1,Move1,Column1),
    replace_board_element(Board,Line1,Column1,Player,TempBoard),
    next_player(Player,NewPlayer),
    display_game(TempBoard,NewPlayer),
    sleep(1),
    smart_move(TempBoard,NewPlayer,Move2),
    (empty_list(Move2)->game_loop(5,NewBoard,NewPlayer);
    nth0(0,Move2,Line2),
    nth0(1,Move2,Column2),
    replace_board_element(TempBoard,Line2,Column2,NewPlayer,NewBoard),
    game_loop(5,NewBoard,Player))).


% inicial state of the board -> all positions are ' '
initial([
    [0,0,0,0,0,0],
    [0,0,0,0,0,0],
    [0,0,0,0,0,0],
    [0,0,0,0,0,0],
    [0,0,0,0,0,0],
    [0,0,0,0,0,0]
    ]).

% checks if the current player can play (if the list of valid moves is empty)
game_over(Board,Player):-
    length(Board,N),
    M is N*N,
    valid_moves(Board,Player,M,LL,F),!,
    empty_list(F).


next_player(1,2).
next_player(2,1).
