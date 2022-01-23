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
		mainMenu
	).


game_mode:-
	printgameModeMenu,
	getChar(Input),
	(
		Input = '1' -> initial_state(GameState), play_pp( 1, GameState);
		Input = '2' -> initial_state(GameState), play_pc( 1, GameState);
		Input = '3' -> initial_state(GameState), play_pc_turn( 1, GameState);
		Input = '4';
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
	write('= Each piece can move one space froward or one space diagonally forward    ='), nl,
	write('=                                                                          ='), nl,
	write('= All pieces capture diagonally.                                           ='), nl,
	write('=                                                                          ='), nl,
	write('= It is not possible to move to the space forward if there is an opponents ='), nl,
	write('= piece                                                                    ='), nl,
	write('============================================================================'), nl,
	press_enter, nl.

