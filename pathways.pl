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
    [0,0,0,0,0,0],
    [0,0,0,0,0,0],
    [0,0,0,0,0,0],
    [0,0,0,0,0,0]
    ]).

test([
    [1,0,0,0,0,1],
    [1,0,0,2,2,2],
    [2,0,0,0,2,0],
    [0,1,1,0,0,1],
    [0,2,0,1,1,0],
    [2,2,0,0,0,1]
    ]).

getLine(0,[H|T],H).
getLine(X,[H|T],[N|Nt]):-
Y is X-1,
getLine(Y,T,[N|Nt]).

getLineElement(0,[H|T],H).
getLineElement(I,[H|T],V):-
Y is I-1,
getLineElement(Y,T,V).

replace([_|T],0,X,[X|T]).
replace([H|T],I,X,[H|R]):-
I1 is I-1,
replace(T,I1,X,R).

replaceBoardElement(Board,LineNumber,ColumnNumber,NewValue,NewBoard):-
getLine(LineNumber,Board,Line),
replace(Line,ColumnNumber,NewValue,NewLine),
replace(Board,LineNumber,NewLine,NewBoard).

getBoardValue(Board,LineNumber,ColumnNumber,Value):-
getLine(LineNumber,Board,Line),
getLineElement(ColumnNumber,Line,Value).

noConnectionsMove(Board,LineNumber,ColumnNumber):-
length(Board,Length),
MaxIndex is Length-1,
CP1 is ColumnNumber+1,
CM1 is ColumnNumber-1,
LP1 is LineNumber+1,
LM1 is LineNumber-1,
((ColumnNumber\==0,
LineNumber\==0,
ColumnNumber\==MaxIndex,
LineNumber\==MaxIndex,
getBoardValue(Board,LineNumber,CP1,V1),
V1==0,
getBoardValue(Board,LineNumber,CM1,V2),
V2==0,
getBoardValue(Board,LP1,ColumnNumber,V3),
V3==0,
getBoardValue(Board,LM1,ColumnNumber,V4),
V4==0);
(ColumnNumber==0,
LineNumber==0,
getBoardValue(Board,LineNumber,CP1,V1),
V1==0,
getBoardValue(Board,LP1,ColumnNumber,V2),
V2==0);
(ColumnNumber==MaxIndex,
LineNumber==MaxIndex,
getBoardValue(Board,LineNumber,CM1,V1),
V1==0,
getBoardValue(Board,LM1,ColumnNumber,V2),
V2==0);
(ColumnNumber==0,
getBoardValue(Board,LineNumber,CP1,V1),
V1==0,
getBoardValue(Board,LP1,ColumnNumber,V2),
V2==0,
getBoardValue(Board,LM1,ColumnNumber,V3),
V3==0);
(ColumnNumber==MaxIndex,
getBoardValue(Board,LineNumber,CM1,V1),
V1==0,
getBoardValue(Board,LP1,ColumnNumber,V2),
V2==0,
getBoardValue(Board,LM1,ColumnNumber,V3),
V3==0);
(LineNumber==0,
getBoardValue(Board,LineNumber,CP1,V1),
V1==0,
getBoardValue(Board,LineNumber,CM1,V2),
V2==0,
getBoardValue(Board,LP1,ColumnNumber,V3),
V3==0);
(LineNumber==MaxIndex,
getBoardValue(Board,LineNumber,CP1,V1),
V1==0,
getBoardValue(Board,LineNumber,CM1,V2),
V2==0,
getBoardValue(Board,LM1,ColumnNumber,V3),
V3==0)).

connectionMove(Board,Player,LineNumber,ColumnNumber):-
length(Board,Length),
MaxIndex is Length-1,
CP1 is ColumnNumber+1,
CM1 is ColumnNumber-1,
LP1 is LineNumber+1,
LM1 is LineNumber-1,
((ColumnNumber\==0,
LineNumber\==0,
ColumnNumber\==MaxIndex,
LineNumber\==MaxIndex,
getBoardValue(Board,LineNumber,CP1,V1),
V1==Player,
getBoardValue(Board,LineNumber,CM1,V2),
V2\==Player,
getBoardValue(Board,LP1,ColumnNumber,V3),
V3\==Player,
getBoardValue(Board,LM1,ColumnNumber,V4),
V4\==Player);
(ColumnNumber\==0,
LineNumber\==0,
ColumnNumber\==MaxIndex,
LineNumber\==MaxIndex,
getBoardValue(Board,LineNumber,CP1,V1),
V1\==Player,
getBoardValue(Board,LineNumber,CM1,V2),
V2==Player,
getBoardValue(Board,LP1,ColumnNumber,V3),
V3\==Player,
getBoardValue(Board,LM1,ColumnNumber,V4),
V4\==Player);
(ColumnNumber\==0,
LineNumber\==0,
ColumnNumber\==MaxIndex,
LineNumber\==MaxIndex,
getBoardValue(Board,LineNumber,CP1,V1),
V1\==Player,
getBoardValue(Board,LineNumber,CM1,V2),
V2\==Player,
getBoardValue(Board,LP1,ColumnNumber,V3),
V3==Player,
getBoardValue(Board,LM1,ColumnNumber,V4),
V4\==Player);
(ColumnNumber\==0,
LineNumber\==0,
ColumnNumber\==MaxIndex,
LineNumber\==MaxIndex,
getBoardValue(Board,LineNumber,CP1,V1),
V1\==Player,
getBoardValue(Board,LineNumber,CM1,V2),
V2\==Player,
getBoardValue(Board,LP1,ColumnNumber,V3),
V3\==Player,
getBoardValue(Board,LM1,ColumnNumber,V4),
V4==Player);
(ColumnNumber==0,
LineNumber==0,
getBoardValue(Board,LineNumber,CP1,V1),
V1==Player,
getBoardValue(Board,LP1,ColumnNumber,V2),
V2\==Player);
(ColumnNumber==0,
LineNumber==0,
getBoardValue(Board,LineNumber,CP1,V1),
V1\==Player,
getBoardValue(Board,LP1,ColumnNumber,V2),
V2==Player);
(ColumnNumber==MaxIndex,
LineNumber==MaxIndex,
getBoardValue(Board,LineNumber,CM1,V1),
V1==Player,
getBoardValue(Board,LM1,ColumnNumber,V2),
V2\==Player);
(ColumnNumber==MaxIndex,
LineNumber==MaxIndex,
getBoardValue(Board,LineNumber,CM1,V1),
V1\==Player,
getBoardValue(Board,LM1,ColumnNumber,V2),
V2==Player);
(ColumnNumber==0,
getBoardValue(Board,LineNumber,CP1,V1),
V1==Player,
getBoardValue(Board,LP1,ColumnNumber,V2),
V2\==Player,
getBoardValue(Board,LM1,ColumnNumber,V3),
V3\==Player);
(ColumnNumber==0,
getBoardValue(Board,LineNumber,CP1,V1),
V1\==Player,
getBoardValue(Board,LP1,ColumnNumber,V2),
V2==Player,
getBoardValue(Board,LM1,ColumnNumber,V3),
V3\==Player);
(ColumnNumber==0,
getBoardValue(Board,LineNumber,CP1,V1),
V1\==Player,
getBoardValue(Board,LP1,ColumnNumber,V2),
V2\==Player,
getBoardValue(Board,LM1,ColumnNumber,V3),
V3==Player);
(ColumnNumber==MaxIndex,
getBoardValue(Board,LineNumber,CM1,V1),
V1==Player,
getBoardValue(Board,LP1,ColumnNumber,V2),
V2\==Player,
getBoardValue(Board,LM1,ColumnNumber,V3),
V3\==Player);
(ColumnNumber==MaxIndex,
getBoardValue(Board,LineNumber,CM1,V1),
V1\==Player,
getBoardValue(Board,LP1,ColumnNumber,V2),
V2==Player,
getBoardValue(Board,LM1,ColumnNumber,V3),
V3\==Player);
(ColumnNumber==MaxIndex,
getBoardValue(Board,LineNumber,CM1,V1),
V1\==Player,
getBoardValue(Board,LP1,ColumnNumber,V2),
V2\==Player,
getBoardValue(Board,LM1,ColumnNumber,V3),
V3==Player);
(LineNumber==0,
getBoardValue(Board,LineNumber,CP1,V1),
V1==Player,
getBoardValue(Board,LineNumber,CM1,V2),
V2\==Player,
getBoardValue(Board,LP1,ColumnNumber,V3),
V3\==Player);
(LineNumber==0,
getBoardValue(Board,LineNumber,CP1,V1),
V1\==Player,
getBoardValue(Board,LineNumber,CM1,V2),
V2==Player,
getBoardValue(Board,LP1,ColumnNumber,V3),
V3\==Player);
(LineNumber==0,
getBoardValue(Board,LineNumber,CP1,V1),
V1\==Player,
getBoardValue(Board,LineNumber,CM1,V2),
V2\==Player,
getBoardValue(Board,LP1,ColumnNumber,V3),
V3==Player);
(LineNumber==MaxIndex,
getBoardValue(Board,LineNumber,CP1,V1),
V1==Player,
getBoardValue(Board,LineNumber,CM1,V2),
V2\==Player,
getBoardValue(Board,LM1,ColumnNumber,V3),
V3\==Player);
(LineNumber==MaxIndex,
getBoardValue(Board,LineNumber,CP1,V1),
V1\==Player,
getBoardValue(Board,LineNumber,CM1,V2),
V2==Player,
getBoardValue(Board,LM1,ColumnNumber,V3),
V3\==Player);
(LineNumber==MaxIndex,
getBoardValue(Board,LineNumber,CP1,V1),
V1\==Player,
getBoardValue(Board,LineNumber,CM1,V2),
V2\==Player,
getBoardValue(Board,LM1,ColumnNumber,V3),
V3==Player)).


validMove(Board,Player,LineNumber,ColumnNumber):-
getBoardValue(Board,LineNumber,ColumnNumber,V),
V==0,
(connectionMove(Board,Player,LineNumber,ColumnNumber);
noConnectionsMove(Board,LineNumber,ColumnNumber)).



smartMove(Board,N,[Line,Column]):-
    length(Board,D),
    NM1 is N-1,
    L is NM1//D,
    C is mod(NM1,D),
    (
        ((validMove(Board,1,L,C),validMove(Board,2,L,C))->[Line,Column]=[L,C]);
        smartMove(Board,NM1,[Line,Column])
    ).

