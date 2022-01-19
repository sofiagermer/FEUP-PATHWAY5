menu :- 
    repeat,
    displayGameTitle,
    readNumber(Option),
    handleMenuChoice(Option).

handleMenuChoice(0).
handleMenuChoice(_) :- write('Invalid Option!'), nl, !, fail.

