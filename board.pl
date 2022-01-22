:- use_module(library(lists)).



columnToInt('A', 0).
columnToInt('B', 1).
columnToInt('C', 2).
columnToInt('D', 3).
columnToInt('E', 4).
columnToInt('F', 5).
columnToInt('G', 6).
columnToInt('H', 7).


display_game(X) :-
    lineNumbers(Y),
    display_game(X,Y).

display_game([],[]) :-
    write('  |---|---|---|---|---|---|---|---|'), nl,
    write('    A   B   C   D   E   F   G   H  '), nl.

display_game([Line|Board],[LineNumb|Remainder]) :-
    write('  |---|---|---|---|---|---|---|---|'), nl,
    %write('  '),  nl,
    write(LineNumb), write(' '),
    printLine(Line),
    write('|'), nl,
    display_game(Board,Remainder).

/* Recursive function to print each board's line */
printLine([]).
printLine([Head|Tail]) :-
    translate(Head,Piece),
    write('|'),
    write(Piece),
    printLine(Tail).



initialBoard([[1,3,3,2,2,3,3,1],
              [1,1,1,1,1,1,1,1],
              [0,0,0,0,0,0,0,0],
              [0,0,0,0,0,0,0,0],
              [0,0,0,0,0,0,0,0],
              [0,0,0,0,0,0,0,0],
              [4,4,4,4,4,4,4,4],
              [4,6,6,5,5,6,6,4]]).


lineNumbers([8,7,6,5,4,3,2,1]).


translate(0,' . ').
translate(1,' B_MT').
translate(2, 'B_HT').
translate(3, 'B_TD').
translate(4, 'W_MT').
translate(5, 'W_HT').
translate(6, 'W_TD').
