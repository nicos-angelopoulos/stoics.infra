/** lib_promise( +PidS, +Load ).

    PidS are promised by Load. That is, no code is loaded until the first call
    to one of the PidS.

*/
lib_promise( Pids, Load ) :-
    is_list( Pids ),
    !,
    maplist( lib_promise_pid(Load), Pids ).
lib_promise( Pid, Load ) :-
    lib_promise_pid( Load, Pid ).

lib_promise_pid( _Load, Promised ) :-
    current_predicate( Promised ),
    !.  % fixme: need to check from where ?
lib_promise_pid( Load, Promised ) :-
    lib_tables:lib_promise( Load, Promised ),
    !.
lib_promise_pid( Load, Promised ) :-
    lib_tables:lib_promise( Other, Promised ),
    Other \== Load,
    !,
    throw( lib(already_promised_from_elsewhere(Promised,Other,Load)) ). % fixme: ... message
lib_promise_pid( Load, Pname/Parity ) :-
    functor( Head, Pname, Parity ),
    assert( (user:Head :- lib:lib_load_promised(Load,Head)) ),
    assert( lib_tables:lib_promise(Load,Pname/Parity) ).

/** lib_load_promised( +Load, +Goal ).

Fullfills the loading of promised predicate and calls goal.<br>
This is the internal called from the dynamically asserted, holding, hot-swapped code.

file test_promise.pl 
==

:- lib(promise(en_list/2,lib(stoics_lib))).

testo :-
    write( start ), nl,
    en_list( x, List ),
    write( en_list_ed(List) ), nl.

?- testo.
start
en_list_ed([x])
true.

==
@author nicos angelopoulos
@version  0.1 2018/1/5

*/
lib_load_promised( Load, Goal ) :-
    forall( lib_tables:lib_promise(Load,Pid),
                    (
                        retract(lib_tables:lib_promise(Load,Pid)),
                        abolish(user:Pid)
                    )
          ),
    ( Load = lib(_) -> call(Load) ; ensure_loaded(Load) ),
    user:call( Goal ).
