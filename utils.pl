:- use_module(library(lists)).


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

