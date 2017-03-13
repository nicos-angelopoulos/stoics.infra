
:- lib( io_get_line/2 ).
:- lib( io_put_line/2 ).

/** io_line( +Stream, ?Line ).

Either get (if Line is a variable), or put a line, (if Line is a list of codes) on Stream.

@author nicos angelopoulos
@version  0.1 2017/3/13    created the common interface for put and get.

*/
io_line( Stream, Line ) :-
    var( Line ),
    !, 
    io_get_line( Stream, Line ).
io_line( Stream, Line ) :-
    Line = [_|_],
    io_put_line( Line, Stream ).
