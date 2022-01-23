
menu(Board,GameMode) :- 
    display_menu_title,
    initial(Board),
    display_playing_mode_options,
    menu_playing_mode_choice(GameMode).


%PLAYING MODE OPTIONS
%loops until user inserts a valid playing mode
menu_playing_mode_choice(GameMode):-
    repeat,
    read_number(PlayingMode),
    handle_menu_playing_mode_choice(PlayingMode,GameMode).

handle_difficulty_choice(1,1,1).
handle_difficulty_choice(2,1,2).
handle_difficulty_choice(1,2,4).
handle_difficulty_choice(2,2,5).

% single player mode
handle_menu_playing_mode_choice(1,GameMode):-
    display_difficulty_options,
    repeat,
    read_number(DifficultyLevel),
    handle_difficulty_choice(DifficultyLevel,1,GameMode).

% multi player mode
handle_menu_playing_mode_choice(2,3).

%bot vs bot mode
%loops until user chooses a valid difficulty level (1 = Normal, 2= Hard)
handle_menu_playing_mode_choice(3,GameMode):-
    display_difficulty_options,
    repeat,
    read_number(DifficultyLevel),
    handle_difficulty_choice(DifficultyLevel,2,GameMode).

%user types wrong playing mode and is asked, again, to insert a valid one (1-3)
handle_menu_playing_mode_choice(_,GameMode):- nl,write('Not a valid mode. Try again: '),fail.
    

%GET NEXT MOVE
%that asks the user the position where he wants to play next
%it loops unitl valid row is inserted
%then it loops until valid column is inserted
%if the move is not valid, the user is asked to insert a valid position
%when a valid input that is valid move is chosen, the board is updated and the game can proceed
move(Board,Player,NewBoard) :- 
    repeat,
    choose_row(Row),
    choose_column(Column),
    ((\+ valid_move(Board,Player,Row,Column))->write('Not a valid move, try again.'),nl,fail;
    replace_board_element(Board,Row,Column,Player,NewBoard),!).

%that reads input from user and checks if it is in the range of the board width
%it loops until the user inserts a valid option
choose_row(HoleRow):-
    display_choose_row,
    repeat,
    read_char(Char),
    ((Char >= 65 , Char < 90, HoleRow is Char-65); (Char >= 97 , Char < 122, HoleRow is Char-97)),
    between(0, 5, HoleRow),!.

%that reads input from user and checks if it is in the range of the board heigh
%it loops until the user inserts a valid option
choose_column(HoleColumn):-
    display_choose_column,
    repeat,
    read_number(Number),
    HoleColumn is Number-1,
    between(0, 5, HoleColumn),!.
