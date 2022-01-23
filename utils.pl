replace([_|T],0,X,[X|T]).
replace([H|T],I,X,[H|R]):-
    I1 is I-1,
    replace(T,I1,X,R).

%given a board, a position(lineNumber, ColumnNumber) and a Value,
%updates the board and retrieves the new Board through the variable NewBoard
replace_board_element(Board,LineNumber,ColumnNumber,NewValue,NewBoard):-
    nth0(LineNumber,Board,Line),
    replace(Line,ColumnNumber,NewValue,NewLine),
    replace(Board,LineNumber,NewLine,NewBoard).

%given a board, and a position(lineNumber, ColumnNumber) 
%retrieves through the variable Value it's position
get_board_value(Board,LineNumber,ColumnNumber,Value):-
    nth0(LineNumber,Board,Line),
    nth0(ColumnNumber,Line,Value).

%checks if list is empty
empty_list([]).

%reads number that user has inserted and returns it's value -1 
%this is done to get the column of the array (column values are form 1-7 and indexes 0-6)
read_number(Number) :-
    get_code(Code),
    Code >= 48, Code < 58,
    Number is Code - 48.

%reads char that user has inserted and returns it's ascii value
read_char(Char) :-
    get_code(Char).