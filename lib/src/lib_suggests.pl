/** lib_suggests( +LibS ).

	For a library or list of libraries
	load the libraries if they are available, else be quiet.

*/
lib_suggests( Libs ) :-
	is_list( Libs ),
	!,
	maplist( lib_suggests, Libs ).
lib_suggests( Lib ) :-
	% % prolog_load_context( module, LoadMod ),
	% that's a bit strange... but seems need to add LoadMod
	% below for it to be used from command line...
	% % catch( LoadMod:ensure_loaded(library(Lib)), _, true ).
	% fixme: be more specific in the catch ?
    % lib( Lib, [mode(suggests)] ).
    catch( lib(Lib,[]), _, fail ),
    !.
lib_suggests( Lib ) :-  % fixme:
    debug( lib, 'Suggested repository no installed: ~w', Lib ).

lib_suggests( Pids, Load ) :-
    is_list( Pids ),
    !,
    maplist( lib_suggests_pid(Load), Pids ).
lib_suggests( Pid, Load ) :-
    lib_suggests_pid( Load, Pid ).

lib_suggests_pid( _Load, Promised ) :-
    current_predicate( Promised ),
    !.  % fixme: need to check from where ?
lib_suggests_pid( Load, Promised ) :-
    lib_tables:lib_promise( Load, Promised ),
    !.
lib_suggests_pid( Load, Promised ) :-
    lib_tables:lib_promise( Other, Promised ),
    Other \== Load,
    !,
    throw( lib(already_promised_from_elsewhere(Promised,Other,Load)) ). % fixme: ... message
lib_suggests_pid( Load, Pname/Parity ) :-
    functor( Head, Pname, Parity ),
    assert( (Head :- lib:lib_promise(Load,Pname/Parity)) ),
    assert( lib_tables:lib_promise(Load,Pname/Parity) ).
