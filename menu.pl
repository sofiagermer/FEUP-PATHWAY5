%:- dynamic handleMenuChoice/1, displayMenuTitle/0, readNumber/1, displayGameTitle/0.

menu(Board) :- 
    displayMenuTitle,
    displayBoardOptions,
    menuChoice(Board).

menuChoice(Board):-
    repeat,
    readNumber(BoardSize),
    handleMenuChoice(BoardSize,Board).

handleMenuChoice(1,Board) :-  
    full(Board).
    handleMenuChoice(2,Board) :- displayBoard8.

nextMove(Board,Player,NewBoard) :-
    repeat,
    chooseRow(Row),
    chooseColumn(Column),
    (validMove(Board,Player,Row,Column)->replaceBoardElement(Board,Row,Column,Player,NewBoard);
    nl,write('Not a valid move, try again.')),nl.


chooseRow(HoleRow):-
    displayChooseRow,
    repeat,
    readChar(Char),
    ((Char >= 65 , Char < 90, HoleRow is Char-65); (Char >= 97 , Char < 122, HoleRow is Char-97)),
    between(0, 5, HoleRow),!.
    % between(0, 5, HoleRow),!.


chooseColumn(HoleColumn):-
    displayChooseColumn,
    repeat,
    readNumber(Number),
    HoleColumn is Number-1,
    between(0, 5, HoleColumn),!.
    % between(0, 5, HoleColumn),!.