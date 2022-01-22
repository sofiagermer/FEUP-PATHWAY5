:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(random)).


:- include('board.pl').       /* Game displaying functions */
:- include('input.pl').       /* Game displaying functions */
:- include('menu.pl').         /* Helper functions */

play :- game.

game :-
    displayGameTitle, 
    menu,
    chooseRow.

initial([
    [0,0,0,0,0,0],
    [0,0,0,0,0,0],
    [2,0,0,0,0,0],
    [0,0,0,0,0,0],
    [0,0,0,0,0,0],
    [0,0,0,0,0,0]
    ]).


getLine(0,[H|T],H).

getLine(X,[H|T],[N|Nt]):-
Y is X-1,
getLine(Y,T,[N|Nt]).

replace([_|T],0,X,[X|T]).
replace([H|T],I,X,[H|R]):-
    I1 is I-1,
    replace(T,I1,X,R).

replaceBoardElement(Board,LineNumber,ColumnNumber,NewValue,NewBoard):-
getLine(LineNumber,Board,Line),
replace(Line,ColumnNumber,NewValue,NewLine),
replace(Board,LineNumber,NewLine,NewBoard).