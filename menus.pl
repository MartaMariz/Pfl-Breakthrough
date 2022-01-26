:- use_module(library(lists)).

main_menu :-
	print_main_menu,
	getChar(Input),
	(
		Input = '1' -> game_mode, main_menu;
		Input = '2' -> print_instructions, main_menu;
		Input = '3';

		nl,	write('Error: invalid input.'), nl,
		press_enter, nl, 
		main_menu
	).


game_mode:-
	printgameModeMenu,
	getChar(Input),
	(
		Input = '1' -> initial_state(GameState), play_pp( 1, GameState);
		Input = '2' -> initial_state(GameState), play_pc( 1, GameState);
		Input = '3' -> initial_state(GameState), play_pc_turn( 1, GameState, 0);
		Input = '4' -> initial_state(GameState), play_pc_turn( 1, GameState, 1);
		Input = '5';
		nl,
		write('Error: invalid input.'), nl,
		press_enter, nl,
		game_mode
	).

printgameModeMenu:-
	write('================================='), nl,
	write('=      ..:: Game Mode ::..      ='), nl,
	write('================================='), nl,
	write('=                               ='), nl,
	write('=   1. Player vs. Player        ='), nl,
	write('=   2. Player vs. Computer      ='), nl,
	write('=   3. Computer vs. Player      ='), nl,
	write('=   4. Computer vs. Computer    ='), nl,
	write('=   5. Back                     ='), nl,
	write('=                               ='), nl,
	write('================================='), nl,
	write('Choose an option:'), nl.

press_enter:-
	write('Press <Enter> to continue.'), nl,
	wait_for_enter, !.

wait_for_enter:-
	get_char(_).

print_main_menu:-
	write('=================================='), nl,
	write('=     ..:: Breakthrough ::..     ='), nl,
	write('=================================='), nl,
	write('=                                ='), nl,
	write('=   1. Play                      ='), nl,
	write('=   2. How to play               ='), nl,
	write('=   3. Exit                      ='), nl,
	write('=                                ='), nl,
	write('=                                ='), nl,
	write('=================================='), nl,
	write('Choose an option:'), nl.



print_instructions:-
	write('============================================================================'), nl,
	write('=                      ..:: How to play ::..                               ='), nl,
	write('============================================================================'), nl,
	write('=                                                                          ='), nl,
	write('= White pieces start.                                                      ='), nl,
	write('=                                                                          ='), nl,
	write('= The goal is to be th first to get to the other side of the board.        ='), nl,
	write('=                                                                          ='), nl,
	write('= Each piece can move one space froward or one space diagonally            ='), nl,
	write('= in each direction forward.                                               ='), nl,
	write('=                                                                          ='), nl,
	write('= Medium Tanks can shoot in the same directions as piece movement          ='), nl,
	write('=                                                                          ='), nl,
	write('= Tank Destroyers can shoot up to two rows forward                         ='), nl,
	write('=                                                                          ='), nl,
	write('= Heavy Tanks can shoot where Medium Tanks shoot and where Tank destroyers ='), nl,
	write('= shoot with an added 2-square diagonal shot in both direction             ='), nl,
	write('=                                                                          ='), nl,
	write('= All pieces move to the destroyed pieces position upon destroying it      ='), nl,
	write('=                                                                          ='), nl,
	write('= It is not possible to move to the space forward if there is an opponents ='), nl,
	write('= piece                                                                    ='), nl,
	write('============================================================================'), nl,
	press_enter, nl.

