
:- use_module(library(lists)).
:- use_module(library(random)).


:- include('board.pl').
:- include('utils.pl').
:- include('menus.pl').



initial_state(GameState) :-
    initialBoard(GameState),
    display_game(GameState).


changePlayer(1,2).
changePlayer(2,1).

check_piece_belong_player(1, 4).
check_piece_belong_player(1, 5).
check_piece_belong_player(1, 6).
check_piece_belong_player(2, 1).
check_piece_belong_player(2, 2).
check_piece_belong_player(2, 3).


check_piece_pos(Column, Row, GameState, Player):- 
    nth0(Row, GameState, RowList),
    nth0(Column, RowList, Piece),
    check_piece_belong_player(Player, Piece).




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



/* checks if a player's piece if in the position CF-RF */
check_if_player_already_piece([CF-RF],Player, GameState):- 
    nth0(RF, GameState, RowList), 
    nth0(CF, RowList, Piece),
    check_if_same_player_piece(Player, Piece).

check_if_same_player_piece(2, Piece ):- dif(1, Piece),  dif(2,Piece),  dif(3, Piece).
check_if_same_player_piece(1, Piece ):-  dif(4, Piece), dif(5,Piece), dif(6, Piece).

/* if tank destroyer moves two rows, check if he didnt pass through one of his own (cant team kill sad) */
check_valid([CI-RI, CF-RF] ,1 , GameState,6, ListOfSpacesToChange):-
    check_if_player_already_piece([CF-RF] ,1 , GameState),
    RF =:= RI - 2,
    RF_1 is RI - 1, 
    check_if_player_already_piece([CF-RF_1] ,1 , GameState),
    check_collumn(CI, CF, 6),
    check_row(RI, RF, 6),
    clone([CI-RI, CI-RF_1], ListOfSpacesToChange), nl.

/* if tank destroyer moves one row*/
check_valid([CI-RI, CF-RF] ,1 , GameState,6, ListOfSpacesToChange):-
    check_if_player_already_piece([CF-RF] ,1 , GameState),
    RF =:= RI - 1,
    check_collumn(CI, CF, 6),
    check_row(RI, RF, 6),
    clone([CI-RI], ListOfSpacesToChange).

/* if tank destroyer moves two rows (black version), check if he didnt pass through one of his own (cant team kill sad) */
check_valid([CI-RI, CF-RF] ,2 , GameState,3, ListOfSpacesToChange):-
    check_if_player_already_piece([CF-RF] ,2, GameState),
    RF =:= RI + 2,
    RF_1 is RI + 1, 
    check_if_player_already_piece([CF-RF_1] ,2 , GameState),
    check_collumn(CI, CF, 3),
    check_row(RI, RF, 3),
    clone([CI-RI, CF-RF_1], ListOfSpacesToChange).


/* if tank destroyer moves one row (black version)*/
check_valid([CI-RI, CF-RF] ,2 , GameState,3, ListOfSpacesToChange):-
    check_if_player_already_piece([CF-RF] ,2 , GameState),
    RF =:= RI + 1,
    check_collumn(CI, CF, 3),
    check_row(RI, RF, 3),
    clone([CI-RI], ListOfSpacesToChange).



check_row(RI, RF, 6):- RF =:= RI - 1.
check_row(RI, RF, 6):- RF =:= RI - 2.
check_collumn(CI, CF, 6):- CF =:= CI.



check_row(RI, RF, 3):- RF =:= RI + 1.
check_row(RI, RF, 3):- RF =:= RI + 2.
check_collumn(CI, CF, 3):- CF =:= CI.

/* if heavy tank moves one row (white version) , perform the same checks as the white medium tank */
check_valid([CI-RI, CF-RF] ,1 , GameState,5, ListOfSpacesToChange):- 
    check_if_player_already_piece([CF-RF],1, GameState),
    RF =:= RI - 1,
    check_valid([CI-RI, CF-RF] ,1 , GameState,4,ListOfSpacesToChange). 

/* if heavy tank moves two rows (white version) in the same column, perform same checks as tank destroyer movement*/
check_valid([CI-RI, CF-RF],1 , GameState,5, ListOfSpacesToChange):-
    check_if_player_already_piece([CF-RF],1, GameState),
    RF =:= RI - 2,
    CF =:= CI,
    check_valid([CI-RI, CF-RF] ,1 , GameState,6, ListOfSpacesToChange). 

/* if heavy tank moves two rows (white version) in diagonal left, check if the piece before is not his own*/
check_valid([CI-RI, CF-RF] ,1 , GameState,5, ListOfSpacesToChange):-
    check_if_player_already_piece([CF-RF],1, GameState),
    RF =:= RI - 2,
    CF =:= CI - 2,
    CF_1 is CI - 1,
    RF_1 is RI - 1,
    check_if_player_already_piece([CF_1-RF_1],1, GameState),
    clone([CI-RI, CF_1-RF_1], ListOfSpacesToChange).

/* if heavy tank moves two rows (white version) in diagonal right, check if the piece before is not his own */
check_valid([CI-RI, CF-RF] ,1 , GameState,5, ListOfSpacesToChange):-
    check_if_player_already_piece([CF-RF],1, GameState),
    RF =:= RI - 2,
    CF =:= CI + 2,
    CF_1 is CI + 1,
    RF_1 is RI - 1,
    check_if_player_already_piece([CF_1-RF_1],1, GameState),
    clone([CI-RI, CF_1-RF_1], ListOfSpacesToChange).

 

/* if heavy tank moves two rows (black version) in the same column, perform same checks as black tank destroyer movement */
check_valid([CI-RI, CF-RF] ,2 , GameState,2, ListOfSpacesToChange):-
    check_if_player_already_piece([CF-RF],2 , GameState),
    RF =:= RI + 2,
    CF =:= CI,
    check_valid([CI-RI, CF-RF] ,2 , GameState,3,ListOfSpacesToChange).

/* if heavy tank moves two rows (black version) in diagonal left, check if the piece before is not his own*/
check_valid([CI-RI, CF-RF] ,2 , GameState,2, ListOfSpacesToChange):-
    check_if_player_already_piece([CF-RF],2, GameState),
    RF =:= RI + 2,
    CF =:= CI - 2,
    CF_1 is CI - 1,
    RF_1 is RI + 1,
    check_if_player_already_piece([CF_1-RF_1],2, GameState),
    clone([CI-RI, CF_1-RF_1], ListOfSpacesToChange).

/* if heavy tank moves two rows (black version) in diagonal right, check if the piece before is not his own*/
check_valid([CI-RI, CF-RF] ,2 , GameState,2, ListOfSpacesToChange):-
    check_if_player_already_piece([CF-RF],2, GameState),
    RF =:= RI + 2,
    CF =:= CI + 2,
    CF_1 is CI + 1,
    RF_1 is RI + 1,
    check_if_player_already_piece([CF_1-RF_1],2, GameState),
    clone([CI-RI, CF_1-RF_1], ListOfSpacesToChange).

/* if heavy tank moves one row (black version) , perform the same checks as the black medium tank */
check_valid([CI-RI, CF-RF] ,2 , GameState,2, ListOfSpacesToChange):-
    check_if_player_already_piece([CF-RF],2 , GameState),
    RF =:= RI + 1,
    check_valid([CI-RI, CF-RF] ,2 , GameState,1,ListOfSpacesToChange).

check_row(RI, RF, 1):- RF =:= RI + 1.
check_row(RI, RF, 4):- RF =:= RI - 1.

check_collumn(CI, CF, 1):- CF =:= CI - 1 .
check_collumn(CI, CF, 1):- CF =:= CI + 1 .
check_collumn(CI, CF, 1):- CF =:= CI.

check_collumn(CI, CF, 4):- CF =:= CI + 1 .
check_collumn(CI, CF, 4):- CF =:= CI - 1 .
check_collumn(CI, CF, 4):- CF =:= CI.

/*  Cases for medium tank of white and black team (piece 1 and 4) */
check_valid([CI-RI, CF-RF] ,1 , GameState, 4,ListOfSpacesToChange):-
    check_if_player_already_piece([CF-RF],1, GameState),
    check_row(RI, RF, 4),
    check_collumn(CI, CF, 4),
    clone([CI-RI], ListOfSpacesToChange).

check_valid([CI-RI, CF-RF] ,2 , GameState, 1, ListOfSpacesToChange):-
    check_if_player_already_piece([CF-RF],2, GameState),
    check_row(RI, RF, 1),
    check_collumn(CI, CF, 1),
    clone([CI-RI], ListOfSpacesToChange).


/*  Replace the pieces he travelled through in case of left diagonal movement */
replace_places_in_between(GameState, 1, MidGameState):-
    RF =:= RI - 2,  
    CF =:= CI - 2,
    CF_1 is CI - 1,
    RF_1 is RI - 1,
    replace(GameState, RF_1, CF_1, 0, MidGameState).

clone([],[]).
clone([H|T],[H|Z]):- clone(T,Z).

replace_places_in_between(GameState,[], MidGameState):- 
    clone(GameState, MidGameState).

replace_places_in_between(GameState,[C-R| T], MidGameState):-
    replace(GameState, R, C, 0, IntermediateGameState),
    replace_places_in_between(IntermediateGameState, T, MidGameState ).

move([CI-RI, CF-RF], Player , GameState, UpdatedGameState):-
    nth0(RI, GameState, RowList), 
    nth0(CI, RowList, Piece),
    check_valid([CI-RI, CF-RF], Player, GameState, Piece, ListOfSpacesToChange),
    replace_places_in_between(GameState,ListOfSpacesToChange,  MidGameState),
    replace(MidGameState, RF, CF, Piece, UpdatedGameState ).    



valid_set_pos(CI,RI,CF,RF,GameState,Player):-
    nth0(RI, GameState, RowList), 
    nth0(CI, RowList, Piece),
    check_piece_pos(CI,RI, GameState, Player),
    check_valid( [CI-RI, CF-RF], Player, GameState, Piece, _).


valid_moves_piece(GameState, Player, [CI-RI], ListOfValidMoves):- 
    nth0(RI, GameState, RowList), 
    nth0(CI, RowList, Piece),
    findall([CI-RI, CF-RF], check_valid([CI-RI, CF-RF], Player,GameState, Piece, _), ListOfValidMoves).



valid_moves(GameState, Player, ListOfMoves):-
    findall([CI-RI, CF-RF], valid_set_pos(CI,RI,CF,RF,GameState,Player), ListOfMoves).

printa([]).
printa([[A-B]|T]):-
    write(A-B),nl,
    printa(T).

printa([[A-B, C-D]|T]):-
    write(A-B),write('  '), write(C-D),nl,
    printa(T).

play:-
    main_menu.

get_move(Player,GameState, [CI-RI, CF-RF]):-
    write('Player '), write(Player), write(', it is your turn!'), nl,
    get_piece_pos(CI, RI),
    check_piece_pos(CI,RI, GameState, Player),
    valid_moves_piece(GameState, Player,[CI-RI], ListOfValidMoves),
    printa(ListOfValidMoves),
    length(ListOfValidMoves, X),
    dif(X, 0),
    get_dest_pos(CF, RF).


check_end([1 | T], 2).
check_end([2 | T], 2).
check_end([3 | T], 2).

check_end([4 | T], 1).
check_end([5 | T], 1).
check_end([6 | T], 1).

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
    write('Player '), write(Winner), write(' won!!'), nl.

play_pc(Player, GameState ):-
    get_move(Player,GameState, Move),
    move(Move ,Player, GameState, UpdatedGameState),
    display_game(UpdatedGameState),
    changePlayer(Player,Oponent),
    play_pc_turn(Oponent, UpdatedGameState, 0).


play_pc_turn(Player, GameState, _):-
    game_over(GameState, Player, Winner),
    dif(Winner, -1),!,
    write('Player '), write(Winner), write(' won!!'), nl.

play_pc_turn(Player, GameState, 0):-
    valid_moves(GameState, Player, ListOfMoves),
    length(ListOfMoves, Length),
    random( 0, Length, Result),
    nth0( Result , ListOfMoves, Move),
    move(Move ,Player, GameState, UpdatedGameState),
    write('It is Player '), write(Player), write( ' turn!' ),nl,
    press_enter,
    display_game(UpdatedGameState),
    changePlayer(Player,Oponent),
    play_pc(Oponent, UpdatedGameState).


play_pc_turn(Player, GameState,1 ):-
    valid_moves(GameState, Player, ListOfMoves),
    length(ListOfMoves, Length),
    random( 0, Length, Result),
    nth0( Result , ListOfMoves, Move),
    move(Move ,Player, GameState, UpdatedGameState),
    write('It is Player '), write(Player), write( ' turn!' ),nl,
    press_enter,
    display_game(UpdatedGameState),
    changePlayer(Player,Oponent),
    play_pc_turn(Oponent, UpdatedGameState, 1).




play_pp(Player, GameState):-
    game_over(GameState, Player, Winner),
    dif(Winner, -1),!,
    write('Player '), write(Winner), write(' won!!'),nl.

play_pp(Player, GameState):-
    get_move(Player,GameState, Move),
    move(Move ,Player, GameState, UpdatedGameState),
    display_game(UpdatedGameState),
    changePlayer(Player,Oponent),
    play_pp(Oponent, UpdatedGameState).

