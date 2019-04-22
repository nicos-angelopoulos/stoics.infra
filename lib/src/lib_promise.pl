/** lib_promise( +PidS, +Load ).

    PidS are promised by Load. That is, no code is loaded until the first call
    to one of the PidS.

*/
lib_promise( Pids, Cxt, Load ) :-
    is_list( Pids ),
    !,
    maplist( lib_promise_pid(Load,Cxt), Pids ).
lib_promise( Pid, Cxt, Load ) :-
    lib_promise_pid( Load, Cxt, Pid ).

lib_promise_pid( _Load, _Cxt, Promised ) :-
    current_predicate( Promised ),
    !.  % fixme: need to check from where ? and Cxt ?
lib_promise_pid( Load, Cxt, Promised ) :-
    lib_tables:lib_promise( Load, Cxt:Promised ),
    !.
lib_promise_pid( Load, Cxt, Promised ) :-
    lib_tables:lib_promise( Other, Cxt:Promised ),
    Other \== Load,
    !,
    throw( lib(already_promised_from_elsewhere(Promised,Other,Load)) ). % fixme: ... message
lib_promise_pid( Load, Cxt, Pname/Parity ) :-
    functor( Head, Pname, Parity ),
    assert( (Cxt:Head :- lib:lib_load_promised(Load,Cxt,Head)) ),
    assert( lib_tables:lib_promise(Load,Cxt:Pname/Parity) ).

/** lib_load_promised( +Load, +Cxt, +Goal ).

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
lib_load_promised( Load, Cxt, Goal ) :-
    forall( lib_tables:lib_promise(Load,Cxt:Pid),
                    (
                        retract(lib_tables:lib_promise(Load,Cxt:Pid)),
                        abolish(Cxt:Pid)
                    )
          ),
    % call(_) was lib(_): changed 19.4.22
    ( Load = call(_) -> Cxt:call(Load) ; 
            lib_defaults(pack,PackDefs), lib(Load,Cxt,PackDefs) ),
    % fixme: throw appropriate error
    call( Cxt:Goal ).
