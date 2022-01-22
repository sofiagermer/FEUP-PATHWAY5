%:- dynamic handleMenuChoice/1, displayMenuTitle/0, readNumber/1, displayGameTitle/0.

menu :- 
    displayMenuTitle,
    displayBoardOptions,
    menuChoice.

menuChoice :-
    repeat,
    readNumber(BoardSize),
    handleMenuChoice(BoardSize).

handleMenuChoice(1) :- displayBoard6.
handleMenuChoice(2) :- displayBoard8.
handleMenuChoice(_) :- write('Invalid Option!'), nl, !, fail.

chooseRow :-
    repeat,
    displayOptionsChoice(0),
    readChar(HoleRow),
    handleHoleRowChoice(HoleRow).

chooseValidRow :-
    repeat,
    displayOptionsChoice(2),
    readChar(HoleRow),
    /*read_char(HoleColumn),*/
    handleHoleRowChoice(HoleRow).

chooseColumn :- 
    repeat,
    displayOptionsChoice(1),
    readNumber(HoleColumn),
    handleHoleColumnChoice(HoleColumn).

chooseValidColumn :- 
    repeat,
    displayOptionsChoice(3),
    readNumber(HoleColumn),
    handleHoleColumnChoice(HoleColumn).

handleHoleRowChoice(41) :- chooseColumn.
handleHoleRowChoice(42) :- chooseColumn.
handleHoleRowChoice(43) :- chooseColumn.
handleHoleRowChoice(44) :- chooseColumn.
handleHoleRowChoice(45) :- chooseColumn.
handleHoleRowChoice(46) :- chooseColumn.
handleHoleRowChoice(97) :- chooseColumn.
handleHoleRowChoice(98) :- chooseColumn.
handleHoleRowChoice(99) :- chooseColumn.
handleHoleRowChoice(100) :- chooseColumn.
handleHoleRowChoice(101) :- chooseColumn.
handleHoleRowChoice(102) :- chooseColumn.
handleHoleRowChoice(_) :- chooseValidRow.

handleHoleColumnChoice(1) :- displayBoard6.
handleHoleColumnChoice(2) :- displayBoard6.
handleHoleColumnChoice(3) :- displayBoard6.
handleHoleColumnChoice(4) :- displayBoard6.
handleHoleColumnChoice(5) :- displayBoard6.
handleHoleColumnChoice(6) :- displayBoard6.
handleHoleColumnChoice(_) :- chooseValidColumn. 