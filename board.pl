:- dynamic getBoardValue/4, displayBoardOptions/0.
displayGameTitle :-
    write('===================================================================================================='), nl,
    write('            ___           ___           ___           ___           ___           ___           ___     '),nl,   
    write('           /\\  \\         /\\  \\         /   \\         /\\__\\         /\\__\\         /\\  \\         |\\__\\   '),nl,
    write('          /::\\  \\       /::\\  \\        \\:\\  \\       /:/  /        /:/ _/_       /::\\  \\        |:|  |   '),nl,
    write('         /:/\\:\\  \\     /:/\\:\\  \\        \\:\\  \\     /:/__/        /:/ /\\__\\     /:/\\:\\  \\       |:|  |   '),nl,
    write('        /::\\~\\:\\  \\   /::\\~\\:\\  \\       /::\\  \\   /::\\  \\ ___   /:/ /:/ _/_   /::\\~\\:\\  \\      |:|__|__'),nl,
    write('       /:/\\:\\ \\:\\__\\ /:/\\:\\ \\:\\__\\     /:/\\:\\__\\ /:/\\:\\  /\\__\\ /:/_/:/ /\\__\\ /:/\\:\\ \\:\\__\\     /::::\\__\\ '),nl,
    write('       \\/__\\:\\/:/  / \\/__\\:\\/:/  /    /:/  \\/__/ \\/__\\:\\/:/  / \\:\\/:/ /:/  / \\/__\\:\\/:/  /    /:/~~/~    '),nl,
    write('            \\::/  /       \\::/  /    /:/  /           \\::/  /   \\::/_/:/  /       \\::/  /    /:/  /     '),nl,
    write('             \\/__/        /:/  /     \\/__/            /:/  /     \\:\\/:/  /        /:/  /     \\/__/      '),nl,
    write('                         /:/  /                      /:/  /       \\::/  /        /:/  /                 '),nl,
    write('                         \\/__/                       \\/__/         \\/__/         \\/__/                  '),nl,nl,
    write(''),nl,
    write(''),nl.

displayMenuTitle :-
    write('========================================================================'), nl,
    write(' '),nl,
    write('             $$\\      $$\\ $$$$$$$$\\ $$\\   $$\\ $$\\   $$\\ '),nl,
    write('             $$$\\    $$$ |$$  _____|$$$\\  $$ |$$ |  $$ |'),nl,
    write('             $$$$\\  $$$$ |$$ |      $$$$\\ $$ |$$ |  $$ |'),nl,
    write('             $$\\$$\\$$ $$ |$$$$$\\    $$ $$\\$$ |$$ |  $$ |'),nl,
    write('             $$ |\\$  /$$ |$$ |      $$ |\\$$$ |$$ |  $$ |'),nl,
    write('             $$ | \\_/ $$ |$$$$$$$$\\ $$ | \\$$ |\\$$$$$$  |'),nl,
    write('             \\__|     \\__|\\________|\\__|  \\__| \\______/ '),nl,
    write(' '),nl,
    write('========================================================================'), nl,
    write(''),nl.

displayGameOver :-
    write('==================================================================================================================='), nl,
    write(' '), nl,
    write('         $$$$$$\\   $$$$$$\\  $$\\      $$\\ $$$$$$$$\\              $$$$$$\\  $$\\    $$\\ $$$$$$$$\\ $$$$$$$\\  '),nl,
    write('         $$ /  \\__|$$ /  $$ |$$$$\\  $$$$ |$$ |                  $$ /  $$ |$$ |   $$ |$$ |      $$ |  $$ |'),nl,
    write('         $$ |$$$$\\ $$$$$$$$ |$$\\$$\\$$ $$ |$$$$$\\                $$ |  $$ |\\$$\\  $$  |$$$$$\\    $$$$$$$  |'),nl,
    write('         $$ |\\_$$ |$$  __$$ |$$ \\$$$  $$ |$$  __|               $$ |  $$ | \\$$\\$$  / $$  __|   $$  __$$< '),nl,
    write('         $$ |\\_$$ |$$  __$$ |$$ \\$$$  $$ |$$  __|               $$ |  $$ | \\$$\\$$  / $$  __|   $$  __$$< '),nl,
    write('         \\$$$$$$  |$$ |  $$ |$$ | \\_/ $$ |$$$$$$$$\\              $$$$$$  |   \\$  /   $$$$$$$$\\ $$ |  $$ |'),nl,
    write('         \\______/ \\__|  \\__|\\__|     \\__|\\________|             \\______/     \\_/    \\________|\\__|  \\__|'),nl,  
    write(' '),nl,
    write('==================================================================================================================='), nl,
    write(' '),nl.
    
displayWinningPlayer(1) :-
    write('======================================================================================================================================'), nl,
    write(' '), nl,
    write('     $$$$$$$\\  $$\\        $$$$$$\\  $$\\     $$\\ $$$$$$$$\\ $$$$$$$\\          $$\\         $$\\      $$\\ $$$$$$\\ $$\\   $$\\  $$$$$$\\  '),nl,
    write('     $$  __$$\\ $$ |      $$  __$$\\ \\$$\\   $$  |$$  _____|$$  __$$\\       $$$$ |        $$ | $\\  $$ |\\_$$  _|$$$\\  $$ |$$  __$$\\ '),nl,
    write('     $$ |  $$ |$$ |      $$ /  $$ | \\$$\\ $$  / $$ |      $$ |  $$ |      \\_$$ |        $$ |$$$\\ $$ |  $$ |  $$$$\\ $$ |$$ /  \\__|'),nl,
    write('     $$$$$$$  |$$ |      $$$$$$$$ |  \\$$$$  /  $$$$$\\    $$$$$$$  |        $$ |        $$ $$ $$\\$$ |  $$ |  $$ $$\\$$ |\\$$$$$$\\  '),nl,
    write('     $$  ____/ $$ |      $$  __$$ |   \\$$  /   $$  __|   $$  __$$<         $$ |        $$$$  _$$$$ |  $$ |  $$ \\$$$$ | \\____$$\\ '),nl,
    write('     $$ |      $$ |      $$ |  $$ |    $$ |    $$ |      $$ |  $$ |        $$ |        $$$  / \\$$$ |  $$ |  $$ |\\$$$ |$$\\   $$ |'),nl,
    write('     $$ |      $$$$$$$$\\ $$ |  $$ |    $$ |    $$$$$$$$\\ $$ |  $$ |      $$$$$$\\       $$  /   \\$$ |$$$$$$\\ $$ | \\$$ |\\$$$$$$  |'),nl,
    write('     \\__|      \\________|\\__|  \\__|    \\__|    \\________|\\__|  \\__|      \\______|      \\__/     \\__|\\______|\\__|  \\__| \\______/ '),nl,
    write(''),nl,
    write('======================================================================================================================================'), nl,
    write(''),nl.

displayWinningPlayer(2) :-
    write('======================================================================================================================================'), nl,
    write(' '), nl,
    write('$$$$$$$\\  $$\\        $$$$$$\\  $$\\     $$\\ $$$$$$$$\\ $$$$$$$\\         $$$$$$\\        $$\\      $$\\ $$$$$$\\ $$\\   $$\\  $$$$$$\\  '), nl,
    write('$$  __$$\\ $$ |      $$  __$$\\ \\$$\\   $$  |$$  _____|$$  __$$\\       $$  __$$\\       $$ | $\\  $$ |\\_$$  _|$$$\\  $$ |$$  __$$\\ '), nl,
    write('$$ |  $$ |$$ |      $$ /  $$ | \\$$\\ $$  / $$ |      $$ |  $$ |      \\__/  $$ |      $$ |$$$\\ $$ |  $$ |  $$$$\\ $$ |$$ /  \\__|'), nl,
    write('$$$$$$$  |$$ |      $$$$$$$$ |  \\$$$$  /  $$$$$\\    $$$$$$$  |       $$$$$$  |      $$ $$ $$\\$$ |  $$ |  $$ $$\\$$ |\\$$$$$$\\  '),nl,
    write('$$  ____/ $$ |      $$  __$$ |   \\$$  /   $$  __|   $$  __$$<       $$  ____/       $$$$  _$$$$ |  $$ |  $$ \\$$$$ | \\____$$\\ '),nl,
    write('$$ |      $$ |      $$ |  $$ |    $$ |    $$ |      $$ |  $$ |      $$ |            $$$  / \\$$$ |  $$ |  $$ |\\$$$ |$$\\   $$ |'),nl,
    write('$$ |      $$$$$$$$\\ $$ |  $$ |    $$ |    $$$$$$$$\\ $$ |  $$ |      $$$$$$$$\\       $$  /   \\$$ |$$$$$$\\ $$ | \\$$ |\\$$$$$$  |'),nl,
    write('\\__|      \\________|\\__|  \\__|    \\__|    \\________|\\__|  \\__|      \\________|      \\__/     \\__|\\______|\\__|  \\__| \\______/ '),nl,
    write(''),nl.

displayBoardOptions :-
    write('                         1. Board 6x6'),nl,
    write(' '), nl,
    write('                         2. Board 8x8'),nl.


displayRowLetter(0) :-  write('A    |||  ').
displayRowLetter(1) :-  write('B    |||  ').
displayRowLetter(2) :-  write('C    |||  ').
displayRowLetter(3) :-  write('D    |||  ').
displayRowLetter(4) :-  write('E    |||  ').
displayRowLetter(5) :-  write('F    |||  ').

displayElement(0) :- write(' ').
displayElement(1) :- write('1').
displayElement(2) :- write('2').


/* getBoardValue(Board,LineNumber,ColumnNumber,Value) */
/*displayNumber(Board,Row,Column,Value) :-*/



displayLine(Board,LineNumber) :-
    displayRowLetter(LineNumber),
    getBoardValue(Board,LineNumber,0,L1),
    displayElement(L1),
    write('  ||  '),
    getBoardValue(Board,LineNumber,1,L2),
    displayElement(L2),
    write('  ||  '),
    getBoardValue(Board,LineNumber,2,L3),
    displayElement(L3),
    write('  ||  '),
    getBoardValue(Board,LineNumber,3,L4),
    displayElement(L4),
    write('  ||  '),
    getBoardValue(Board,LineNumber,4,L5),
    displayElement(L5),
    write('  ||  '),
    getBoardValue(Board,LineNumber,5,L6),
    displayElement(L6),
    write('  |||').

displayBoard6Template(Board):-
    write(' '),nl,
    write('          1      2      3      4      5      6  '),nl,
    write('        _____  _____  _____  _____  _____  _____'),nl,
    write('     |||     ||     ||     ||     ||     ||     |||'     ),nl,
    displayLine(Board,0),nl,
    write('     |||_____||_____||_____||_____||_____||_____|||'     ),nl,
    write('     |||     ||     ||     ||     ||     ||     |||'     ),nl,
    displayLine(Board,1),nl,
    write('     |||_____||_____||_____||_____||_____||_____|||'     ),nl,
    write('     |||     ||     ||     ||     ||     ||     |||'     ),nl,
    displayLine(Board,2),nl,
    write('     |||_____||_____||_____||_____||_____||_____|||'     ),nl,
    write('     |||     ||     ||     ||     ||     ||     |||'     ),nl,
    displayLine(Board,3),nl,
    write('     |||_____||_____||_____||_____||_____||_____|||'     ),nl,
    write('     |||     ||     ||     ||     ||     ||     |||'     ),nl,
    displayLine(Board,4),nl,
    write('     |||_____||_____||_____||_____||_____||_____|||'     ),nl,
    write('     |||     ||     ||     ||     ||     ||     |||'     ),nl,
    displayLine(Board,5),nl,
    write('     |||_____||_____||_____||_____||_____||_____|||'     ),nl,
    write(' '),nl,
    write(' =============================================== '),nl. 

 displayBoard6(Board,1) :-
    write(' ====================================================== '),nl,
    write('                       Player 1 Turn'),nl,
    write(' ====================================================== '),nl,
    displayBoard6Template(Board).

displayBoard6(Board,2) :-
    write(' ====================================================== '),nl,
    write('                       Player 2 Turn'),nl,
    write(' ====================================================== '),nl,
    displayBoard6Template(Board).

displayBoard8 :-
    write(' ============================================================ '),nl,
    write(' '),nl,
    write('        ____  ____  ____  ____  ____  ____  ____  ____'),nl,
    write('     |||    ||    ||    ||    ||    ||    ||    ||    |||'),nl,
    write('     |||    ||    ||    ||    ||    ||    ||    ||    |||'),nl,
    write('     |||____||____||____||____||____||____||____||____|||'),nl,
    write('     |||    ||    ||    ||    ||    ||    ||    ||    |||'),nl,
    write('     |||    ||    ||    ||    ||    ||    ||    ||    |||'),nl,
    write('     |||____||____||____||____||____||____||____||____|||'),nl,
    write('     |||    ||    ||    ||    ||    ||    ||    ||    |||'),nl,
    write('     |||    ||    ||    ||    ||    ||    ||    ||    |||'),nl,
    write('     |||____||____||____||____||____||____||____||____|||'),nl,
    write('     |||    ||    ||    ||    ||    ||    ||    ||    |||'),nl,
    write('     |||    ||    ||    ||    ||    ||    ||    ||    |||'),nl,
    write('     |||____||____||____||____||____||____||____||____|||'),nl,
    write('     |||    ||    ||    ||    ||    ||    ||    ||    |||'),nl,
    write('     |||    ||    ||    ||    ||    ||    ||    ||    |||'),nl,
    write('     |||____||____||____||____||____||____||____||____|||'),nl,
    write('     |||    ||    ||    ||    ||    ||    ||    ||    |||'),nl,
    write('     |||    ||    ||    ||    ||    ||    ||    ||    |||'),nl,
    write('     |||____||____||____||____||____||____||____||____|||'),nl,
    write('     |||    ||    ||    ||    ||    ||    ||    ||    |||'),nl,
    write('     |||    ||    ||    ||    ||    ||    ||    ||    |||'),nl,
    write('     |||____||____||____||____||____||____||____||____|||'),nl,
    write('     |||    ||    ||    ||    ||    ||    ||    ||    |||'),nl,
    write('     |||    ||    ||    ||    ||    ||    ||    ||    |||'),nl,
    write('     |||____||____||____||____||____||____||____||____|||'),nl,
    write(' '),nl,
    write(' ============================================================ '),nl.


 /* Normal Pergunta Row*/
displayChooseRow :-
    write(' =============================================== '),nl,
    write('     IN WHICH ROW DO YOU WANT TO PLAY (A-F)?'),nl,
    write(' =============================================== '),nl,
    write(' '), nl.
 /* Normal Pergunta Column*/
displayChooseColumn :-
    write(' =============================================== '),nl,
    write('   IN WHICH COLUMN DO YOU WANT TO PLAY (1-6)?'),nl,
    write(' =============================================== '),nl,
    write(' '), nl.
