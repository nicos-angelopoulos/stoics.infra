/** io_close( +FileR, -Stream ).

If FileR is a stream (should be identical to Stream) then do nothing.
Else, close Stream.

*/
io_close( FileR, Stream ) :-
	is_stream( FileR ),
    FileR == Stream,
	!.
io_close( _FileR, Stream ) :-
	close( Stream ).
