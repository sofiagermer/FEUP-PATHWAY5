replace([_|T],0,X,[X|T]).
replace([H|T],I,X,[H|R]):-
    I1 is I-1,
    replace(T,I1,X,R).

replace_board_element(Board,LineNumber,ColumnNumber,NewValue,NewBoard):-
    nth0(LineNumber,Board,Line),
    replace(Line,ColumnNumber,NewValue,NewLine),
    replace(Board,LineNumber,NewLine,NewBoard).

get_board_value(Board,LineNumber,ColumnNumber,Value):-
    nth0(LineNumber,Board,Line),
    nth0(ColumnNumber,Line,Value).

empty_list([]).

read_number(Number) :-
    get_code(Code),
    Code >= 48, Code < 58,
    Number is Code - 48.

read_char(Char) :-
    get_code(Char).