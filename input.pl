readNumber(Number) :-
    get_code(Code), skip_line,
    Code >= 48, Code < 58,
    Number is Code - 48.

readChar(Char) :-
    get0(Code), skip_line,
    ((Code >= 65, Code < 90); (Code >= 97, Code < 122)),
    Char is Code.
