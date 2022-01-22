%:- dynamic handleMenuChoice/1, displayMenuTitle/0, readNumber/1, displayGameTitle/0.

menu :- 
    displayMenuTitle,
    displayBoardOptions,
    menuChoice.

menuChoice :-
    repeat,
    readNumber(BoardSize),
    handleMenuChoice(BoardSize).

handleMenuChoice(1) :-  chooseRow(initial).
handleMenuChoice(2) :- displayBoard8.
handleMenuChoice(_) :- write('Invalid Option!'), nl, !, fail.

chooseRow(Board) :-
    repeat,
    displayOptionsChoice(0),
    readChar(HoleRow),
    handleHoleRowChoice(HoleRow, Board).

chooseValidRow :-
    repeat,
    displayOptionsChoice(2),
    readChar(HoleRow),
    /*read_char(HoleColumn),*/
    handleHoleRowChoice(HoleRow).

chooseColumn(Board, Row) :- 
    repeat,
    displayOptionsChoice(1),
    readNumber(HoleColumn),
    write('inside choose column'),nl,
    write(HoleColumn),nl,
    NovoHole is HoleColumn-1,
    write(NovoHole),nl,
    handleHoleColumnChoice(NovoHole, Board,Row).

chooseValidColumn :- 
    repeat,
    displayOptionsChoice(3),
    readNumber(HoleColumn),
    handleHoleColumnChoice(HoleColumn).

handleHoleRowChoice(0,Board) :- chooseColumn(Board, 0).
handleHoleRowChoice(1,Board) :- chooseColumn(Board, 1).
handleHoleRowChoice(2,Board) :- chooseColumn(Board, 2).
handleHoleRowChoice(3,Board) :- chooseColumn(Board, 3).
handleHoleRowChoice(4,Board) :- chooseColumn(Board, 4).
handleHoleRowChoice(5,Board) :- chooseColumn(Board, 5).
handleHoleRowChoice(_) :- chooseValidRow. 

handleHoleColumnChoice(0,Board, Row) :- 
    write('handle hole column 0'),
    replaceBoardElement(Board,Row,1,1,NewBoard),
    displayBoard6(NewBoard).
handleHoleColumnChoice(1,Board, Row) :- 
    write('handle hole column 1'),
    replaceBoardElement(Board,Row,1,1,NewBoard), 
    displayBoard6(NewBoard).
handleHoleColumnChoice(2,Boar, Row) :- 
    write('handle hole column 2'),
    replaceBoardElement(Board,Row,1,1,NewBoard), 
    displayBoard6(NewBoard).
handleHoleColumnChoice(3,Board, Row) :- 
    write('handle hole column 3'),
    replaceBoardElement(Board,Row,1,1,NewBoard), 
    displayBoard6(NewBoard).
handleHoleColumnChoice(4,Board, Row) :- 
    write('handle hole column 4'),
    replaceBoardElement(Board,Row,1,1,NewBoard), 
    displayBoard6(NewBoard).
handleHoleColumnChoice(5,Board, Row) :- 
    write('handle hole column 5'),
    replaceBoardElement(Board,Row,1,1,NewBoard), 
    displayBoard6(NewBoard).
handleHoleColumnChoice(_, Board, Row) :- 
    chooseValidColumn(). 
