no_connections_move(Board,LineNumber,ColumnNumber):-
    length(Board,Length),
    MaxIndex is Length-1,
    CP1 is ColumnNumber+1,
    CM1 is ColumnNumber-1,
    LP1 is LineNumber+1,
    LM1 is LineNumber-1,
    ((ColumnNumber\==0,
    LineNumber\==0,
    ColumnNumber\==MaxIndex,
    LineNumber\==MaxIndex,
    get_board_value(Board,LineNumber,CP1,V1),
    V1==0,
    get_board_value(Board,LineNumber,CM1,V2),
    V2==0,
    get_board_value(Board,LP1,ColumnNumber,V3),
    V3==0,
    get_board_value(Board,LM1,ColumnNumber,V4),
    V4==0);
    (ColumnNumber==0,
    LineNumber==0,
    get_board_value(Board,LineNumber,CP1,V1),
    V1==0,
    get_board_value(Board,LP1,ColumnNumber,V2),
    V2==0);
    (ColumnNumber==MaxIndex,
    LineNumber==MaxIndex,
    get_board_value(Board,LineNumber,CM1,V1),
    V1==0,
    get_board_value(Board,LM1,ColumnNumber,V2),
    V2==0);
    (ColumnNumber==MaxIndex,
    LineNumber==0,
    get_board_value(Board,LineNumber,CM1,V1),
    V1==0,
    get_board_value(Board,LP1,ColumnNumber,V2),
    V2==0);
    (ColumnNumber==0,
    LineNumber==MaxIndex,
    get_board_value(Board,LineNumber,CP1,V1),
    V1==0,
    get_board_value(Board,LM1,ColumnNumber,V2),
    V2==0);
    (ColumnNumber==0,
    get_board_value(Board,LineNumber,CP1,V1),
    V1==0,
    get_board_value(Board,LP1,ColumnNumber,V2),
    V2==0,
    get_board_value(Board,LM1,ColumnNumber,V3),
    V3==0);
    (ColumnNumber==MaxIndex,
    get_board_value(Board,LineNumber,CM1,V1),
    V1==0,
    get_board_value(Board,LP1,ColumnNumber,V2),
    V2==0,
    get_board_value(Board,LM1,ColumnNumber,V3),
    V3==0);
    (LineNumber==0,
    get_board_value(Board,LineNumber,CP1,V1),
    V1==0,
    get_board_value(Board,LineNumber,CM1,V2),
    V2==0,
    get_board_value(Board,LP1,ColumnNumber,V3),
    V3==0);
    (LineNumber==MaxIndex,
    get_board_value(Board,LineNumber,CP1,V1),
    V1==0,
    get_board_value(Board,LineNumber,CM1,V2),
    V2==0,
    get_board_value(Board,LM1,ColumnNumber,V3),
    V3==0)).

connection_move(Board,Player,LineNumber,ColumnNumber):-
    length(Board,Length),
    MaxIndex is Length-1,
    CP1 is ColumnNumber+1,
    CM1 is ColumnNumber-1,
    LP1 is LineNumber+1,
    LM1 is LineNumber-1,
    ((ColumnNumber\==0,
    LineNumber\==0,
    ColumnNumber\==MaxIndex,
    LineNumber\==MaxIndex,
    get_board_value(Board,LineNumber,CP1,V1),
    V1==Player,
    get_board_value(Board,LineNumber,CM1,V2),
    V2\==Player,
    get_board_value(Board,LP1,ColumnNumber,V3),
    V3\==Player,
    get_board_value(Board,LM1,ColumnNumber,V4),
    V4\==Player);
    (ColumnNumber\==0,
    LineNumber\==0,
    ColumnNumber\==MaxIndex,
    LineNumber\==MaxIndex,
    get_board_value(Board,LineNumber,CP1,V1),
    V1\==Player,
    get_board_value(Board,LineNumber,CM1,V2),
    V2==Player,
    get_board_value(Board,LP1,ColumnNumber,V3),
    V3\==Player,
    get_board_value(Board,LM1,ColumnNumber,V4),
    V4\==Player);
    (ColumnNumber\==0,
    LineNumber\==0,
    ColumnNumber\==MaxIndex,
    LineNumber\==MaxIndex,
    get_board_value(Board,LineNumber,CP1,V1),
    V1\==Player,
    get_board_value(Board,LineNumber,CM1,V2),
    V2\==Player,
    get_board_value(Board,LP1,ColumnNumber,V3),
    V3==Player,
    get_board_value(Board,LM1,ColumnNumber,V4),
    V4\==Player);
    (ColumnNumber\==0,
    LineNumber\==0,
    ColumnNumber\==MaxIndex,
    LineNumber\==MaxIndex,
    get_board_value(Board,LineNumber,CP1,V1),
    V1\==Player,
    get_board_value(Board,LineNumber,CM1,V2),
    V2\==Player,
    get_board_value(Board,LP1,ColumnNumber,V3),
    V3\==Player,
    get_board_value(Board,LM1,ColumnNumber,V4),
    V4==Player);
    (ColumnNumber==0,
    LineNumber==0,
    get_board_value(Board,LineNumber,CP1,V1),
    V1==Player,
    get_board_value(Board,LP1,ColumnNumber,V2),
    V2\==Player);
    (ColumnNumber==0,
    LineNumber==0,
    get_board_value(Board,LineNumber,CP1,V1),
    V1\==Player,
    get_board_value(Board,LP1,ColumnNumber,V2),
    V2==Player);
    (ColumnNumber==MaxIndex,
    LineNumber==MaxIndex,
    get_board_value(Board,LineNumber,CM1,V1),
    V1==Player,
    get_board_value(Board,LM1,ColumnNumber,V2),
    V2\==Player);
    (ColumnNumber==MaxIndex,
    LineNumber==MaxIndex,
    get_board_value(Board,LineNumber,CM1,V1),
    V1\==Player,
    get_board_value(Board,LM1,ColumnNumber,V2),
    V2==Player);
    (ColumnNumber==MaxIndex,
    LineNumber==0,
    get_board_value(Board,LineNumber,CM1,V1),
    V1\==Player,
    get_board_value(Board,LP1,ColumnNumber,V2),
    V2==Player);
    (ColumnNumber==0,
    LineNumber==MaxIndex,
    get_board_value(Board,LineNumber,CP1,V1),
    V1\==Player,
    get_board_value(Board,LM1,ColumnNumber,V2),
    V2==Player);
    (ColumnNumber==0,
    get_board_value(Board,LineNumber,CP1,V1),
    V1==Player,
    get_board_value(Board,LP1,ColumnNumber,V2),
    V2\==Player,
    get_board_value(Board,LM1,ColumnNumber,V3),
    V3\==Player);
    (ColumnNumber==0,
    get_board_value(Board,LineNumber,CP1,V1),
    V1\==Player,
    get_board_value(Board,LP1,ColumnNumber,V2),
    V2==Player,
    get_board_value(Board,LM1,ColumnNumber,V3),
    V3\==Player);
    (ColumnNumber==0,
    get_board_value(Board,LineNumber,CP1,V1),
    V1\==Player,
    get_board_value(Board,LP1,ColumnNumber,V2),
    V2\==Player,
    get_board_value(Board,LM1,ColumnNumber,V3),
    V3==Player);
    (ColumnNumber==MaxIndex,
    get_board_value(Board,LineNumber,CM1,V1),
    V1==Player,
    get_board_value(Board,LP1,ColumnNumber,V2),
    V2\==Player,
    get_board_value(Board,LM1,ColumnNumber,V3),
    V3\==Player);
    (ColumnNumber==MaxIndex,
    get_board_value(Board,LineNumber,CM1,V1),
    V1\==Player,
    get_board_value(Board,LP1,ColumnNumber,V2),
    V2==Player,
    get_board_value(Board,LM1,ColumnNumber,V3),
    V3\==Player);
    (ColumnNumber==MaxIndex,
    get_board_value(Board,LineNumber,CM1,V1),
    V1\==Player,
    get_board_value(Board,LP1,ColumnNumber,V2),
    V2\==Player,
    get_board_value(Board,LM1,ColumnNumber,V3),
    V3==Player);
    (LineNumber==0,
    get_board_value(Board,LineNumber,CP1,V1),
    V1==Player,
    get_board_value(Board,LineNumber,CM1,V2),
    V2\==Player,
    get_board_value(Board,LP1,ColumnNumber,V3),
    V3\==Player);
    (LineNumber==0,
    get_board_value(Board,LineNumber,CP1,V1),
    V1\==Player,
    get_board_value(Board,LineNumber,CM1,V2),
    V2==Player,
    get_board_value(Board,LP1,ColumnNumber,V3),
    V3\==Player);
    (LineNumber==0,
    get_board_value(Board,LineNumber,CP1,V1),
    V1\==Player,
    get_board_value(Board,LineNumber,CM1,V2),
    V2\==Player,
    get_board_value(Board,LP1,ColumnNumber,V3),
    V3==Player);
    (LineNumber==MaxIndex,
    get_board_value(Board,LineNumber,CP1,V1),
    V1==Player,
    get_board_value(Board,LineNumber,CM1,V2),
    V2\==Player,
    get_board_value(Board,LM1,ColumnNumber,V3),
    V3\==Player);
    (LineNumber==MaxIndex,
    get_board_value(Board,LineNumber,CP1,V1),
    V1\==Player,
    get_board_value(Board,LineNumber,CM1,V2),
    V2==Player,
    get_board_value(Board,LM1,ColumnNumber,V3),
    V3\==Player);
    (LineNumber==MaxIndex,
    get_board_value(Board,LineNumber,CP1,V1),
    V1\==Player,
    get_board_value(Board,LineNumber,CM1,V2),
    V2\==Player,
    get_board_value(Board,LM1,ColumnNumber,V3),
    V3==Player)).


valid_move(Board,Player,LineNumber,ColumnNumber):-
    get_board_value(Board,LineNumber,ColumnNumber,V),
    V==0,
    (no_connections_move(Board,LineNumber,ColumnNumber);
    connection_move(Board,Player,LineNumber,ColumnNumber)).

valid_moves(Board,Player,0,LL,F):-
    F=LL.

valid_moves(Board,Player,N,LL,F):-
    length(Board,D),
    NM1 is N-1,
    L is NM1//D,
    C is mod(NM1,D),
    ((valid_move(Board,Player,L,C)->append([[L,C]],LL,NLL));
    NLL=LL),
    valid_moves(Board,Player,NM1,NLL,F).

% used for both levels of intelligence
%  picks a random move from the list of valid moves
random_move(Board,Player,Move):-
    length(Board,N),
    M is N*N,
    valid_moves(Board,Player,M,LL,MovesList),
    length(MovesList,UpperLimit),
    random(0,UpperLimit,RValue),
    nth0(RValue,MovesList,Move).
   
% used only in the highest level of intelligence
%  first checks whether there is a smart move to be made, and if there isn't it chooses the random move
smart_moves(Board,[],LL,F):-
    F=LL.

smart_moves(Board,[H|T],LL,F):-
    nth0(0,H,L),
    nth0(1,H,C),
    (((valid_move(Board,1,L,C),valid_move(Board,2,L,C))->append([H],LL,NLL));
    NLL=LL),
    smart_moves(Board,T,NLL,F).

smart_move(Board,Player,Move):-
    length(Board,N),
    M is N*N,
    valid_moves(Board,Player,M,L1,MovesList),
    smart_moves(Board,MovesList,L2,SmartMovesList),
    (empty_list(SmartMovesList)->random_move(Board,Player,Move);
    length(SmartMovesList,UpperLimit),
    random(0,UpperLimit,RValue),
    nth0(RValue,SmartMovesList,Move)).
