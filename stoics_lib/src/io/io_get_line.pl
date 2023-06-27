/** io_get_line( +Stream, -Line ).
	
Gets next line from Stream. Line is a list of Codes. 
The new line is not returned in Line. Returns end_of_file at end of file. 

==
?- atom_codes(abc,Abc), open(abc.txt,write,Out), io_put_line(Abc,Out),close(Out).
?- open(abc.txt,read,In), io_get_line(In,Line), atom_codes(Atom,Line),close(In).
Atom = abc.
==

@author nicos angelopoulos
@version  0.1 2016/12/9
@version  0.2 2023/6/27,    allow for files that end without EOL- by removing end_of_file from last list
@see fget_line/2

*/
io_get_line( Stream, Cs ) :-
	get_code( Stream, C ),
     ( (C =:= -1;C =:= 0'\n) -> Cs = end_of_file
                              ; io_get_line(C, Stream,Cs)
     ).

io_get_line( -1, _Stream, Cs ) :- !, Cs = [].
io_get_line( 0'\n, _Stream, Cs ) :- !, Cs = [].
io_get_line( C, Stream, [C|Cs] ) :-
	get_code( Stream, NxC ),
	io_get_line( NxC, Stream, Cs ).
