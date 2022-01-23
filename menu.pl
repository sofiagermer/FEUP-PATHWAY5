%:- dynamic handleMenuChoice/1, displayMenuTitle/0, readNumber/1, displayGameTitle/0.

menu(NewBoard) :- 
    displayMenuTitle,
    displayBoardOptions,
    menuChoice(NewBoard).

menuChoice(NewBoard):-
    repeat,
    readNumber(BoardSize),
    handleMenuChoice(BoardSize,NewBoard).

handleMenuChoice(1,NewBoard) :-  
    displayOptionsChoice(0),
    nextMove([
    [0,0,0,0,0,0],
    [0,0,0,0,0,0],
    [0,0,0,0,0,0],
    [0,0,0,0,0,0],
    [0,0,0,0,0,0],
    [0,0,0,0,0,0]
    ],NewBoard).
handleMenuChoice(2) :- displayBoard8.
handleMenuChoice(_) :- write('Invalid Option!'), nl, !, fail.

nextMove(Board,NewBoard) :-
    chooseRow(Row),
    displayOptionsChoice(1),
    chooseColumn(Column),
    replaceBoardElement(Board,Row,Column,1,NewBoard),
    displayBoard6(NewBoard).

chooseRow(X):-
    repeat,
    readChar(HoleRow),
    between(0, 5, HoleRow).
    % between(0, 5, HoleRow),!.

chooseColumn(X):-
    repeat,
    readNumber(HoleColumn),
    between(0, 5, HoleColumn).
    % between(0, 5, HoleColumn),!.