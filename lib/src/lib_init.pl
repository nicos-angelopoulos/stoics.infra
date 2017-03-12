/** lib_init( +Goal ).
    lib_init( +Goal, +Cxt ).

  Call Goal in current lib loading context.

  Is not run when lib_mkindex/2 is used.

*/

lib_init( Goal ) :-
    lib_loading_context( Cxt ),
    lib_init( Goal, Cxt ).

lib_init( Goal, Cxt ) :-
    call( Cxt:Goal ).
