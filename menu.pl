%:- dynamic handleMenuChoice/1, displayMenuTitle/0, readNumber/1, displayGameTitle/0.

game :-
    displayGameTitle, 
    menu.

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


