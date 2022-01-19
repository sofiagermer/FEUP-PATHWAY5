:- dynamic handleMenuChoice/1, displayMenuTitle/0, readNumber/1, displayGameTitle/0.

game :-
    displayGameTitle, 
    displayMenuTitle,
    menu.

menu :- 
    repeat,
    %displayMenuTitle,
    %displayBoardOptions,
    readNumber(Option),
    handleMenuChoice(Option).


