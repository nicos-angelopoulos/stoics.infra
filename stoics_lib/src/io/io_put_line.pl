/**  io_put_line( +Codes, +Stream ).

	Output a line of Codes onto Stream.

@author nicos angelopoulos
@version  0.1 2016/12/9
@see fput_line/2.

*/

io_put_line( [], Stream ) :-
	!,
	put( Stream, 10 ).
io_put_line( [C|Cs], Stream ) :-
	put( Stream, C ),
	io_put_line( Cs, Stream ).
