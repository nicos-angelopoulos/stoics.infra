
:- lib( io_get_line/2 ).
:- lib( io_put_line/2 ).
:- lib( io_open/3 ).
:- lib( io_close/2 ).

/** io_lines( +FileOrStream, -Lines ).
    io_lines( +FileOrStream, +Lines ).

	Read/write a list of lines from/to a file or stream. Each line is a list of codes.
	When Lines is ground, writing to file/stream is assumed.
	If FileOrStream corresponds to a current stream, this is used for I/O. Else
	FileOrStream is taken to be a file which is opened in correct mode. 
	In the latter case the stream is closing at the end of operation, whereas streams
	are left open.

==
?- maplist( atom_codes, [abc,edf,xyz], Lines ), io_lines( test_out.txt, Lines ).
==

@author nicos angelopoulos
@version 1.0  2016/12/09
@see file_to_list_lines/2 and list_of_lines_to_file/2
@see io_open/3, io_close/2.

*/
io_lines( FileR, Lines ) :-
	ground( Lines ),
	!,
	io_open( FileR, write, Out ),
	io_put_lines( Lines, Out ),
	io_close( FileR, Out ).
	
io_lines( FileR, Lines ) :-
	io_open( FileR, read, Stream ),
	io_get_line( Stream, Line ),
     io_get_lines( Line, Stream, Lines ),
     io_close( FileR, Stream ).

io_get_lines( end_of_file, _Stream, [] ) :- !.
io_get_lines( Line, Stream, [Line|Ls] ) :-
	io_get_line( Stream, New ),
	io_get_lines( New, Stream, Ls ).

io_put_lines( [], _Out ).
io_put_lines( [L|Ls], Out ) :-
	io_put_line_put( L, Out ),
	io_put_lines( Ls, Out ).
