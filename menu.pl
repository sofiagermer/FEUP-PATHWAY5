%:- dynamic handleMenuChoice/1, displayMenuTitle/0, readNumber/1, displayGameTitle/0.

menu(Board) :- 
    displayMenuTitle,
    %displayBoardOptions,
    displayPlayingModeOptions,
    menuPlayingModeChoice(Board).
    %menuBoardChoice(Board).

% BOARD OPTIONS
menuBoardChoice(Board):-
    repeat,
    readNumber(BoardSize),
    handleMenuBoardChoice(BoardSize,Board).

handleMenuBoardChoice(1,Board) :-  
    full(Board).
    handleMenuChoice(2,Board) :- displayBoard8.

%PLAYING MODE OPTIONS
menuPlayingModeChoice(Board):-
    repeat,
    readNumber(PlayingMode),
    handleMenuPlayingModeChoice(PlayingMode,Board).

% single player
handleMenuPlayingModeChoice(1,Board) :-
    full(Board).

%GET NEXT MOVE
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