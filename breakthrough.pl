
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


check_piece_pos(Column, Row, GameState, Player):- 
    nth0(Row, GameState, RowList),
    nth0(Column, RowList, Piece),
    Player =:= Piece.

    

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
    main_menu.

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
    write('It is Player '), write(Player), write( ' turn!' ),nl,
    press_enter,
    display_game(UpdatedGameState),
    changePlayer(Player,Oponent),
    play_pc(Oponent, UpdatedGameState).


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

