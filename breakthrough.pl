
:- use_module(library(lists)).
:- use_module(library(random)).

columnToInt('A', 0).
columnToInt('B', 1).
columnToInt('C', 2).
columnToInt('D', 3).
columnToInt('E', 4).
columnToInt('F', 5).
columnToInt('G', 6).
columnToInt('H', 7).



initial_state(GameState) :-
    initialBoard(GameState),
    display_game(GameState).


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

get_piece_pos(Column, Row):-
    repeat,
    write('Choose which piece to move.'), nl,
	write('Column:'),
	get_char(Char),skip_line,
	columnToInt(Char, Column),nl,
	write('Row:'),
	getInt(R),
	R =< 8,
	R >= 1,
	Row is 8-R.



get_dest_pos(Column, Row):-
    repeat,
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


check_piece_pos(Column, Row, GameState, Player):- 
    nth0(Row, GameState, RowList),
    nth0(Column, RowList, Piece),
    Player =:= Piece.

    
initialPlayer(1).

replace( [L|Ls] , 0 , C , Piece , [R|Ls] ) :- 
  replace_row(L,C,Piece,R).                                      
replace( [L|Ls] , R , C , Piece , [L|Rs] ) :- 
  R > 0 ,                                
  R1 is R-1 ,                             
  replace( Ls , R1 , C , Piece , Rs ).                                       

replace_row( [_|Cs] , 0 , Piece , [Piece|Cs] ) .  
replace_row( [H|Cs] , C , Piece , [H|Rs] ) :- 
  C > 0 ,                                  
  C1 is C-1 ,                                
  replace_row( Cs , C1 , Piece , Rs ). 

check_collumn(CI, CF, _):- CF =:= CI - 1 .
check_collumn(CI, CF, _):- CF =:= CI + 1 .
check_collumn(CI, CF, Piece):- CF =:= CI, Piece =:= 0 .

check_valid([CI-RI, CF-RF] ,1 , GameState):-
    nth0(RF, GameState, RowList),
    nth0(CF, RowList, Piece),
    dif(1, Piece),
    RF =:= RI - 1,
    check_collumn(CI, CF, Piece).

check_valid([CI-RI, CF-RF] ,2 , GameState):-
    nth0(RF, GameState, RowList),
    nth0(CF, RowList, Piece),
    dif(2, Piece),
    check_collumn(CI, CF, Piece),
    RF =:= RI + 1.

move([CI-RI, CF-RF],Player , GameState, UpdatedGameState):-
    check_valid([CI-RI, CF-RF], Player, GameState),
    replace(GameState, RF, CF, Player, MidGameState ),
    replace(MidGameState, RI, CI, 0, UpdatedGameState).    


valid_set_pos(CI,RI,CF,RF,GameState,Player):-
    check_piece_pos(CI,RI, GameState, Player),
    check_valid( [CI-RI, CF-RF], Player, GameState).



valid_moves(GameState, Player, ListOfMoves):-
    findall([CI-RI, CF-RF], valid_set_pos(CI,RI,CF,RF,GameState,Player), ListOfMoves).

printa([]).
printa([[A-B, C-D]|T]):-
    write(A-B), write(C-D),nl,
    printa(T).

play:-
    initial_state(GameState),
    initialPlayer(Player),
    play_pc(Player, GameState).

get_move(Player,GameState, [CI-RI, CF-RF]):-
    write('Player '), write(Player), write(', it is your turn!'), nl,
    get_piece_pos(CI, RI),
    check_piece_pos(CI,RI, GameState, Player),
    get_dest_pos(CF, RF).


check_end([H | T], Player):-
    H =:= Player.

check_end([H | T], Player):-
    check_end( T, Player).

check_end([], Player):- fail.


game_over([First|_], 2, Winner):-
    check_end(First, 1),!,
    Winner is  1.

game_over(GameState, 1, Winner):-
    length(GameState, NumRows),
    LastRow is NumRows-1,
    nth0( LastRow , GameState, RowList),
    check_end(RowList, 2),!,
    Winner is 2.

game_over(_, _, Winner):-
    Winner is -1.


play_pc(Player, GameState):-
    game_over(GameState, Player, Winner),
    dif(Winner, -1),!,
    write('Player '), write(Winner), write(' won!!').

play_pc(Player, GameState):-
    get_move(Player,GameState, Move),
    move(Move ,Player, GameState, UpdatedGameState),
    display_game(UpdatedGameState),
    changePlayer(Player,Oponent),
    play_pc_turn(Oponent, UpdatedGameState).


play_pc_turn(Player, GameState):-
    game_over(GameState, Player, Winner),
    dif(Winner, -1),!,
    write('Player '), write(Winner), write(' won!!').

play_pc_turn(Player, GameState):-
    valid_moves(GameState, Player, ListOfMoves),
    length(ListOfMoves, Length),
    random( 0, Length, Result),
    nth0( Result , ListOfMoves, Move),
    move(Move ,Player, GameState, UpdatedGameState),
    display_game(UpdatedGameState),
    changePlayer(Player,Oponent),
    play_pc(Oponent, UpdatedGameState).


play_pp(Player, GameState):-
    game_over(GameState, Player, Winner),
    dif(Winner, -1),!,
    write('Player '), write(Winner), write(' won!!').

play_pp(Player, GameState):-
    get_move(Player,GameState, Move),
    move(Move ,Player, GameState, UpdatedGameState),
    display_game(UpdatedGameState),
    changePlayer(Player,Oponent),
    play_pp(Oponent, UpdatedGameState).

