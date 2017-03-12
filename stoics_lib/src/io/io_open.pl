/** io_open( +FileR, +Mode, -Stream ).

	If FileR is a stream, just unify it to Stream, else assume is a file,
	and open for access in Mode.

*/
io_open( FileR, _Mode, Stream ) :-
	is_stream( FileR ),
	!,
	Stream = FileR.
io_open( FileR, Mode, Stream ) :-
	open( FileR, Mode, Stream ).
