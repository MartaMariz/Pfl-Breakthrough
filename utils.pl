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



printa([]).
printa([[A-B]|T]):-
    write(A-B),nl,
    printa(T).

printa([[A-B, C-D]|T]):-
    write(A-B),write('  '), write(C-D),nl,
    printa(T).

clone([],[]).
clone([H|T],[H|Z]):- clone(T,Z).