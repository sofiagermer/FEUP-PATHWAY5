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

emptyList([]).
