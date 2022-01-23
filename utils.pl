get_line(0,[H|T],H).
get_line(X,[H|T],[N|Nt]):-
    Y is X-1,
    get_line(Y,T,[N|Nt]).

get_line_element(0,[H|T],H).
get_line_element(I,[H|T],V):-
    Y is I-1,
    get_line_element(Y,T,V).

replace([_|T],0,X,[X|T]).
replace([H|T],I,X,[H|R]):-
    I1 is I-1,
    replace(T,I1,X,R).

replace_board_element(Board,LineNumber,ColumnNumber,NewValue,NewBoard):-
    get_line(LineNumber,Board,Line),
    replace(Line,ColumnNumber,NewValue,NewLine),
    replace(Board,LineNumber,NewLine,NewBoard).

get_board_value(Board,LineNumber,ColumnNumber,Value):-
    get_line(LineNumber,Board,Line),
    get_line_element(ColumnNumber,Line,Value).

empty_list([]).

read_number(Number) :-
    get_code(Code),
    Code >= 48, Code < 58,
    Number is Code - 48.

read_char(Char) :-
    get_code(Char).