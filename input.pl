readNumber(Number) :-
    get_code(Code), skip_line,
    Code >= 48, Code < 58,
    Number is Code - 48.

readLetter(Letter) :-
    get_code(Code), skip_line,

readBoardPosition :-
