
:- use_module(library(lists)).

columnToInt('A', 0).
columnToInt('B', 1).
columnToInt('C', 2).
columnToInt('D', 3).
columnToInt('E', 4).
columnToInt('F', 5).
columnToInt('G', 6).
columnToInt('H', 7).



printInitialBoard :-
    initialBoard(X),
    lineNumbers(Y),
    printBoard(X,Y).



printBoard([],[]) :-
    write('  |---|---|---|---|---|---|---|---|'), nl,
    write('    A   B   C   D   E   F   G   H  '), nl.

printBoard([Line|Board],[LineNumb|Remainder]) :-
    write('  |---|---|---|---|---|---|---|---|'), nl,
    %write('  '),  nl,
    write(LineNumb), write(' '),
    printLine(Line),
    write('|'), nl,
    printBoard(Board,Remainder).

/* Recursive function to print each board's line */
printLine([]).
printLine([Head|Tail]) :-
    translate(Head,Piece),
    write('|'),
    write(Piece),
    printLine(Tail).


initialBoard([[2,2,2,2,2,2,2,2],
              [2,2,2,2,2,2,2,2],
              [0,0,1,0,0,0,0,0],
              [0,0,0,0,0,0,0,0],
              [0,0,0,0,0,0,0,0],
              [0,0,0,0,0,0,0,0],
              [1,1,0,1,1,1,1,1],
              [1,1,1,1,1,1,1,1]]).


lineNumbers([8,7,6,5,4,3,2,1]).


translate(0,' . ').
translate(1,' W '). 
translate(2,' B '). 

getPiecePos(Column, Row):-
    write('Choose which piece to move.'), nl,
	write('Column:'),
	get_char(Char),skip_line,
	columnToInt(Char, Column),nl,
	write('Row:'),
	getInt(R),
	R =< 8,
	R >= 1,
	Row is 8-R.



getDestPos(Column, Row):-
    write('Choose where to move.'), nl,
	write('Column:'),
	get_char(Char),skip_line,
	columnToInt(Char, Column),
	write('Row:'),
	getInt(R),
	R =< 8,
	R >= 1,
	Row is 8-R.

getInt(Input):-
	get_code(TempInput),skip_line,
	Input is TempInput - 48,nl.

getChar(Input):-
	get_char(Input),
  write(Input),nl,
	get_char(_).


changePlayer(1,2).
changePlayer(2,1).


checkPiecePos(Column, Row, GameState, Player):- 
    nth0(Row, GameState, RowList),
    nth0(Column, RowList, Piece),
    Player =:= Piece.

    
initialPlayer(1).


checkResult(Piece, _ , _, Player, _, _):- Piece =:= Player ,!.
checkResult(Piece, Row, Col, Player,  GameState, UpdatedGameState):-
    replace(GameState, Row, Col, Player, UpdatedGameState ).


replace( [L|Ls] , 0 , C , Piece , [R|Ls] ) :- 
  replaceRow(L,C,Piece,R).                                      
replace( [L|Ls] , R , C , Piece , [L|Rs] ) :- 
  R > 0 ,                                
  R1 is R-1 ,                             
  replace( Ls , R1 , C , Piece , Rs ).                                       

replaceRow( [_|Cs] , 0 , Piece , [Piece|Cs] ) .  
replaceRow( [H|Cs] , C , Piece , [H|Rs] ) :- 
  C > 0 ,                                  
  C1 is C-1 ,                                
  replaceRow( Cs , C1 , Piece , Rs ). 

checkCollumn(CI, CF,_):- CF =:= CI - 1.
checkCollumn(CI, CF,_):- CF =:= CI + 1.
checkCollumn(CI, CF, Piece):- CF =:= CI, Piece =:= 0 .


checkMove(CI, RI, CF, RF,1, GameState, UpdatedGameState):-
    nth0(RF, GameState, RowList),
    nth0(CF, RowList, Piece),
    RF =:= RI - 1,
    checkCollumn(CI, CF, Piece),
    checkResult(Piece, RF, CF, 1, GameState, FinalState),
    replace(FinalState, RI, CI, 0, UpdatedGameState).    

checkMove(CI, RI, CF, RF,2, GameState, UpdatedGameState):-
    RF =:= RI + 1,
    checkCollumn(CI, CF),
    nth0(RF, GameState, RowList),
    nth0(CF, RowList, Piece),
    checkResult(Piece, RF, CF, 2, GameState, FinalState),
    replace(FinalState, RI, CI, 0, UpdatedGameState). 



play:-
    initialBoard(GameState),
    lineNumbers(Y),
    printBoard(GameState, Y),
    initialPlayer(Player),
    move(Player, GameState, Y).
    


move(Player, GameState, Y):-
    write('Player '), write(Player), write(', it is your turn!'), nl,
    getPiecePos(CI, RI),
    checkPiecePos(CI,RI, GameState, Player),
    getDestPos(CF, RF),
    checkMove(CI, RI, CF, RF,Player, GameState, UpdatedGameState),
    printBoard(UpdatedGameState,Y),
    changePlayer(Player,Oponent),
    move(Oponent, UpdatedGameState, Y).

