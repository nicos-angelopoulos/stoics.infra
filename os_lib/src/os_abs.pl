/** os_abs( +Os, -Abs ).
    os_abs( +Os, -Abs, +Opts ).

Short for absolute_file_name/2 but also when Os is '' it is not interpreted as '.'. 
Os can a / starting slash Os term (os_name/2).
Note that absolute_file_name/2 deals correctly with all other os_name/2 types.

*/
os_abs( Os, Abs ) :-
    os_abs( Os, Abs, [] ).

os_abs( '', '', _ ) :- !.
os_abs( /(Sub), Abs, OptsPrvT ) :-
	!,
	os_term( Atom, Sub ),
	atomic_concat( '/', Atom, AbsSub ),
    ( is_list(OptsPrvT) -> OptsPrv = OptsPrvT; OptsPrv = [OptsPrvT] ),
    ( select(access(new),OptsPrv,Opts) ->
        ( absolute_file_name(AbsSub,Abs,[access(exist),file_errors(fail)|Opts]) ->
            exists_file(Abs),
            throw(error(permission_error(overwrite,'existing file',Abs),_))   % use 'context' instead of _ to surpress trace
            ;
            true
        )
        ;
	    absolute_file_name( AbsSub, Abs, OptsPrv )
    ).

os_abs( Rel, Abs, OptsPrvT ) :-
    ( is_list(OptsPrvT) -> OptsPrv = OptsPrvT; OptsPrv = [OptsPrvT] ),
    ( select(access(new),OptsPrv,Opts) ->
        ( absolute_file_name(Rel,Abs,[access(exist),file_errors(fail)|Opts]) ->
            exists_file( Abs ),
            throw(error(permission_error(overwrite,'existing file',Abs),_))
            ;
            true
        )
        ;
	    absolute_file_name( Rel, Abs, OptsPrv )
    ).
