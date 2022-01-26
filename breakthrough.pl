
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

/* attributes to each piece its corresponding value (used for board evaluation) */
get_piece_value(1,1).
get_piece_value(2,5).
get_piece_value(3,2).
get_piece_value(4,1).
get_piece_value(5,5).
get_piece_value(6,2).

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



/* checks if a player's piece if in the position CF-RF */
check_if_player_has_piece_pos([CF-RF],Player, GameState):- 
    nth0(RF, GameState, RowList), 
    nth0(CF, RowList, Piece),
    check_piece_belong_player(Player, Piece).

/* checks if theres no piece in the position CF-RF */
check_if_empty([CF-RF], GameState):- 
    nth0(RF, GameState, RowList), 
    nth0(CF, RowList, Piece),
    Piece =:= 0.

/* checks if the opponent has a piece if in the position CF-RF */
check_if_opponent_or_empty([CF-RF],Player, GameState):-  
    nth0(RF, GameState, RowList), 
    nth0(CF, RowList, Piece),
    check_piece_belong_opponent_or_empty(Player, Piece).


check_piece_belong_opponent_or_empty(1,Piece):- dif(4, Piece), dif(5, Piece), dif(6, Piece).
check_piece_belong_opponent_or_empty(2,Piece):- dif(1, Piece), dif(2, Piece), dif(3, Piece).

/*  valid rows and columns for the different pieces */

check_row(RI, RF, 6):- RF =:= RI - 1.
check_row(RI, RF, 6):- RF =:= RI - 2.


check_row(RI, RF, 1):- RF =:= RI + 1.
check_row(RI, RF, 4):- RF =:= RI - 1.
check_row(RI, RF, 3):- RF =:= RI + 1.
check_row(RI, RF, 3):- RF =:= RI + 2.

check_collumn(CI, CF, 1):- CF =:= CI - 1 .
check_collumn(CI, CF, 1):- CF =:= CI + 1 .
check_collumn(CI, CF, 1):- CF =:= CI.

check_collumn(CI, CF, 4):- CF =:= CI + 1 .
check_collumn(CI, CF, 4):- CF =:= CI - 1 .
check_collumn(CI, CF, 4):- CF =:= CI.
check_collumn(CI, CF, 6):- CF =:= CI.
check_collumn(CI, CF, 3):- CF =:= CI.

/*  Check valid moves for tank destroyer of white team (piece 6) */

/* if tank destroyer moves two rows, check if he didnt pass through one of his own (cant team kill sad) and that there is something for him to kill */
check_valid([CI-RI, CF-RF] ,1 , GameState,6, ListOfSpacesToChange):-
    check_if_player_has_piece_pos([CF-RF] ,2 , GameState), %check that there is an enemy for him to kill here
    RF =:= RI - 2,
    RF_1 is RI - 1, 
    check_if_opponent_or_empty([CF-RF_1] ,1 , GameState),
    check_collumn(CI, CF, 6),
    check_row(RI, RF, 6),
    clone([CI-RI, CI-RF_1], ListOfSpacesToChange), nl.

/* if tank destroyer moves one row diagonnaly (can only do so into empty squares) */
check_valid([CI-RI, CF-RF] ,1 , GameState,6, ListOfSpacesToChange):-
    check_if_empty([CF-RF] , GameState),
    RF =:= RI - 1,
    CI =\= CF,
    check_valid([CI-RI, CF-RF] ,1 , GameState,4, ListOfSpacesToChange).

/* if tank destroyer moves one row (in front, can always do)*/
check_valid([CI-RI, CF-RF] ,1 , GameState,6, ListOfSpacesToChange):-
    check_if_opponent_or_empty([CF-RF], 1, GameState),
    RF =:= RI - 1,
    CI =:= CF,
    clone([CI-RI], ListOfSpacesToChange).


/*  Check valid moves for tank destroyer of black team (piece 3) */

/* if tank destroyer moves two rows (black version), check if he didnt pass through one of his own (cant team kill sad) */
check_valid([CI-RI, CF-RF] ,2 , GameState,3, ListOfSpacesToChange):-
    check_if_player_has_piece_pos([CF-RF] ,1 , GameState), %check that there is an enemy for him to kill here
    RF =:= RI + 2,
    RF_1 is RI + 1, 
    check_if_opponent_or_empty([CF-RF_1] ,2 , GameState),
    check_collumn(CI, CF, 3),
    check_row(RI, RF, 3),
    clone([CI-RI, CF-RF_1], ListOfSpacesToChange).


/* if tank destroyer (black version) moves one row diagonnaly (can only do so into empty squares) */
check_valid([CI-RI, CF-RF] ,2 , GameState,3, ListOfSpacesToChange):-
    check_if_empty([CF-RF] , GameState),
    RF =:= RI + 1,
    CI =\= CF,
    check_valid([CI-RI, CF-RF] ,2 , GameState,1, ListOfSpacesToChange).

/* if tank destroyer (black version) moves one row (in front, can always do)*/
check_valid([CI-RI, CF-RF] ,2 , GameState,3, ListOfSpacesToChange):-
    check_if_opponent_or_empty([CF-RF], 2, GameState),
    RF =:= RI + 1,
    CI =:= CF,
    clone([CI-RI], ListOfSpacesToChange).


/*  Check valid moves for heavy tank of white team (piece 5) */

/* if heavy tank moves one row (white version) , perform the same checks as the white medium tank */
check_valid([CI-RI, CF-RF] ,1 , GameState,5, ListOfSpacesToChange):- 
    check_if_opponent_or_empty([CF-RF],1, GameState),
    RF =:= RI - 1,
    check_valid([CI-RI, CF-RF] ,1 , GameState,4,ListOfSpacesToChange). 

/* if heavy tank moves two rows (white version) in the same column, perform same checks as tank destroyer movement*/
check_valid([CI-RI, CF-RF],1 , GameState,5, ListOfSpacesToChange):-
    check_if_opponent_or_empty([CF-RF],1, GameState),
    RF =:= RI - 2,
    CF =:= CI,
    check_valid([CI-RI, CF-RF] ,1 , GameState,6, ListOfSpacesToChange). 

/* if heavy tank moves two rows (white version) in diagonal left, check if the piece before is not his own and that he has something to kill there*/
check_valid([CI-RI, CF-RF] ,1 , GameState,5, ListOfSpacesToChange):-
    check_if_opponent_or_empty([CF-RF],1, GameState),
    check_if_player_has_piece_pos([CF-RF],2,GameState ),
    RF =:= RI - 2,
    CF =:= CI - 2,
    CF_1 is CI - 1,
    RF_1 is RI - 1,
    check_if_opponent_or_empty([CF_1-RF_1],1, GameState),
    clone([CI-RI, CF_1-RF_1], ListOfSpacesToChange).

/* if heavy tank moves two rows (white version) in diagonal right, check if the piece before is not his own and that he has something to kill there */
check_valid([CI-RI, CF-RF] ,1 , GameState,5, ListOfSpacesToChange):-
    check_if_opponent_or_empty([CF-RF],1, GameState),
    check_if_player_has_piece_pos([CF-RF],2,GameState ),
    RF =:= RI - 2,
    CF =:= CI + 2,
    CF_1 is CI + 1,
    RF_1 is RI - 1,
    check_if_opponent_or_empty([CF_1-RF_1],1, GameState),
    clone([CI-RI, CF_1-RF_1], ListOfSpacesToChange).

 

/*  Check valid moves for heavy tank of black team (piece 2) */

/* if heavy tank moves two rows (black version) in the same column, perform same checks as black tank destroyer movement */
check_valid([CI-RI, CF-RF] ,2 , GameState,2, ListOfSpacesToChange):-
    check_if_opponent_or_empty([CF-RF],2 , GameState),
    RF =:= RI + 2,
    CF =:= CI,
    check_valid([CI-RI, CF-RF] ,2 , GameState,3,ListOfSpacesToChange).

/* if heavy tank moves two rows (black version) in diagonal left, check if the piece before is not his own and that player 1 has something there for him to kill*/
check_valid([CI-RI, CF-RF] ,2 , GameState,2, ListOfSpacesToChange):-
    check_if_opponent_or_empty([CF-RF],2, GameState),
    check_if_player_has_piece_pos([CF-RF],1,GameState ),
    RF =:= RI + 2,
    CF =:= CI - 2,
    CF_1 is CI - 1,
    RF_1 is RI + 1,
    check_if_opponent_or_empty([CF_1-RF_1],2, GameState),
    clone([CI-RI, CF_1-RF_1], ListOfSpacesToChange).

/* if heavy tank moves two rows (black version) in diagonal right, check if the piece before is not his own and that player 1 has something there for him to kill*/
check_valid([CI-RI, CF-RF] ,2 , GameState,2, ListOfSpacesToChange):-
    check_if_opponent_or_empty([CF-RF],2, GameState),
    check_if_player_has_piece_pos([CF-RF],1,GameState),
    RF =:= RI + 2,
    CF =:= CI + 2,
    CF_1 is CI + 1,
    RF_1 is RI + 1,
    check_if_opponent_or_empty([CF_1-RF_1],2, GameState),
    clone([CI-RI, CF_1-RF_1], ListOfSpacesToChange).

/* if heavy tank moves one row (black version) , perform the same checks as the black medium tank */
check_valid([CI-RI, CF-RF] ,2 , GameState,2, ListOfSpacesToChange):-
    check_if_opponent_or_empty([CF-RF],2 , GameState),
    RF =:= RI + 1,
    check_valid([CI-RI, CF-RF] ,2 , GameState,1,ListOfSpacesToChange).


/*  Check valid moves for medium tank of white team (piece 4) */
check_valid([CI-RI, CF-RF] ,1 , GameState, 4,ListOfSpacesToChange):-
    check_if_opponent_or_empty([CF-RF],1, GameState),
    check_row(RI, RF, 4),
    check_collumn(CI, CF, 4),
    clone([CI-RI], ListOfSpacesToChange).

/*  Check valid moves for medium tank of black team (piece 1) */
check_valid([CI-RI, CF-RF] ,2 , GameState, 1, ListOfSpacesToChange):-
    check_if_opponent_or_empty([CF-RF],2, GameState),
    check_row(RI, RF, 1),
    check_collumn(CI, CF, 1),
    clone([CI-RI], ListOfSpacesToChange).


/*  iterates through a list of positions and makes that square empty */
replace_places_in_between(GameState,[], MidGameState):- 
    clone(GameState, MidGameState).

replace_places_in_between(GameState,[C-R| T], MidGameState):-
    replace(GameState, R, C, 0, IntermediateGameState),
    replace_places_in_between(IntermediateGameState, T, MidGameState ).

/*  executes the given play, modifying the game board itself */
move([CI-RI, CF-RF], Player , GameState, UpdatedGameState):-
    nth0(RI, GameState, RowList), 
    nth0(CI, RowList, Piece),
    check_valid([CI-RI, CF-RF], Player, GameState, Piece, ListOfSpacesToChange),
    replace_places_in_between(GameState,ListOfSpacesToChange,  MidGameState),
    replace(MidGameState, RF, CF, Piece, UpdatedGameState ).    


/*  Return all of the valid moves for ALL of this player's pieces on the board */
valid_set_pos(CI,RI,CF,RF,GameState,Player):-
    nth0(RI, GameState, RowList), 
    nth0(CI, RowList, Piece),
    check_piece_pos(CI,RI, GameState, Player),
    check_valid( [CI-RI, CF-RF], Player, GameState, Piece, _).


valid_moves_piece(GameState, Player, [CI-RI], ListOfValidMoves):- 
    nth0(RI, GameState, RowList), 
    nth0(CI, RowList, Piece),
    findall([CI-RI, CF-RF], check_valid([CI-RI, CF-RF], Player,GameState, Piece, _), ListOfValidMoves).

/*  Return all of the valid moves for ALL of this player's pieces on the board */
valid_moves(GameState, Player, ListOfMoves):-
    findall([CI-RI, CF-RF], valid_set_pos(CI,RI,CF,RF,GameState,Player), ListOfMoves).


/*  Iterate through all rows and cols and check how many pieces each one has */
num_of_pieces(_, [], 0).

num_of_pieces(Player, [H | T], Num_Of_Pieces):-
    check_individual_row(Player, H, Num_of_Pieces_Row),
    num_of_pieces(Player, T, Num_of_Pieces_Next),
    Num_Of_Pieces is Num_of_Pieces_Next + Num_of_Pieces_Row.


check_individual_row(_, [], 0).

check_individual_row(Player, [H | T], Num_Of_Pieces):-
    check_piece_belong_player(Player, H),
    get_piece_value(H, Piece_Val),
    check_individual_row(Player, T, Num_of_Pieces_Next),
    Num_Of_Pieces is Num_of_Pieces_Next + Piece_Val.

check_individual_row(Player, [_ | T], Num_Of_Pieces):-
    check_individual_row(Player, T, Num_Of_Pieces).

/*  Display board evaluation for Player */
printa_game_value(GameState, Player):- 
    value(GameState, Player, Val),
    write('Current game value for Player '), write(Player), write(' is: '), write(Val), nl.

/*  Display all valid plays */
printa_valid_moves([]):- write('The above are your valid moves!'), 
    nl.
    
printa_valid_moves([[A-B, C-D]|T]):-
    columnToInt(I_COL, A),
    columnToInt(F_COL, C),
    I_ROW is 8 - B,
    F_ROW is  8 - D,
    write(I_COL-I_ROW),write('       '), write(F_COL-F_ROW),nl,
    printa_valid_moves(T).

/*  Give an estimated evaluation of the current board situation for one of the players */
value(GameState, 1, Value):- 
    num_of_pieces(1, GameState, Pieces_Player1),
    num_of_pieces(2, GameState, Pieces_Player2),
    Value is Pieces_Player1 - Pieces_Player2.

value(GameState, 2, Value):- 
    num_of_pieces(1, GameState, Pieces_Player1),
    num_of_pieces(2, GameState, Pieces_Player2),
    Value is Pieces_Player2 - Pieces_Player1.


/* starts the game by displaying the main menu */
play:-
    main_menu.

/* gets the input from the user for his move and also displays what moves he can make, as well as the current board evaluation */
get_move(Player,GameState, [CI-RI, CF-RF]):-
    write('Player '), write(Player), write(', it is your turn!'), nl,
    get_piece_pos(CI, RI),
    check_piece_pos(CI,RI, GameState, Player),
    valid_moves_piece(GameState, Player,[CI-RI], ListOfValidMoves),
    printa_valid_moves(ListOfValidMoves),
    length(ListOfValidMoves, X),
    dif(X, 0),
    get_dest_pos(CF, RF).


check_end([1 | _], 2).
check_end([2 | _], 2).
check_end([3 | _], 2).

check_end([4 | _], 1).
check_end([5 | _], 1).
check_end([6 | _], 1).

check_end([_ | T], Player):-
    check_end( T, Player).

check_end([], _):- fail.


/*checks if a player has reached the other end of the board with one of his pieces */
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


/* player vs PC AND PC vs PC main function */
play_pc(Player, GameState):-
    game_over(GameState, Player, Winner),
    dif(Winner, -1),!,
    write('Player '), write(Winner), write(' won!!'), nl.

play_pc(Player, GameState ):-
    printa_game_value(GameState, Player),
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
    printa_game_value(GameState, Player),
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
    printa_game_value(GameState, Player),
    press_enter,
    display_game(UpdatedGameState),
    changePlayer(Player,Oponent),
    play_pc_turn(Oponent, UpdatedGameState, 1).


/* Player vs player main function */
play_pp(Player, GameState):-
    game_over(GameState, Player, Winner),
    dif(Winner, -1),!,
    write('Player '), write(Winner), write(' won!!'),nl.

play_pp(Player, GameState):-
    printa_game_value(GameState, Player),
    get_move(Player,GameState, Move),
    move(Move ,Player, GameState, UpdatedGameState),
    display_game(UpdatedGameState),
    changePlayer(Player,Oponent),
    play_pp(Oponent, UpdatedGameState).

