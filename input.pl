readNumber(Number) :-
    get_code(Code),
    Code >= 48, Code < 58,
    Number is Code - 48.

readChar(Char) :-
    get0(Char).