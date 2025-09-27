/** lib_promise( +Tid, +Did, +Cxt, +Load ).

    Pid is a predicate id that triggers promised code loaded by Load. That is, no code is loaded until the first call
    to Pid. Did is the dependant predicate (its F/A id), which is not used by lib_promise/4 for loading.
     
    Currently Dep is not used or checked. Could be later on be part of reporting why things are promised reports.

==
:- lib(promise(testo/2,pesto/0,real)).

% Loads system library real, when testo/2 is executed which contains a reference to undefined pesto/0.

:- lib(promise(resto/2,nesto/0,r(pheatmap))).

% Loads R library pheatmpa, when resto/2 is called which contains a reference to undefined nesto/0.

==

@author nicos angelopoulos
@version 0:2 2025/09/27

*/
lib_promise( Pids, Cxt, Load ) :-
    is_list( Pids ),
    !,
    maplist( lib_promise_pid(Load,Cxt), Pids ).
lib_promise( Pid, Cxt, Load ) :-
    lib_promise_pid( Load, Cxt, Pid ).

lib_promise_pid( Load, _Cxt, r(Function) ) :-
    !,
    lib_promise_pid_r( Load, Function ).
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

lib_promise_pid_r( Load, Function ) :-
    lib_tables:lib_promise_r( Load, Function ),
    !.
lib_promise_pid_r( Load, Function ) :-
    lib_tables:lib_promise_r( Else, Function ),
    !,
    Else \== Load,
    throw( lib(r_already_promised_from_elsewhere(Function,Else,Load)) ). % fixme: ... message
lib_promise_pid_r( Load, Function ) :-
    assert( lib_tables:lib_promise_r(Load,Function) ).

/** lib_load_promised( +Load, +Cxt, +Goal ).

Fullfills the loading of promised predicate and calls goal.<br>
This is the internal called from the dynamically asserted, holding, hot-swapped code.

The predicate will only call each unique Cxt:Gid, where Gid is Goal's PredID, once.

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
@version  0.2 2025/9/27

*/
lib_load_promised( Load, Cxt, Goal ) :-
    forall( lib_tables:lib_promise(Load,Cxt:Pid),
                    (
                        retract(lib_tables:lib_promise(Load,Cxt:Pid)),
                        abolish(Cxt:Pid)
                    )
          ),
    % call(_) was lib(_): changed 19.4.22
    Mess = 'Loading promised code with: ~w, failed.',
    Rep1 = lib_message_report(Mess, [With], informational ),
    ( Load = call(_) -> 
            With = Load,
            catch(Cxt:call(Load),_,Rep1) 
            ; 
            lib_defaults(pack,PackDefs),
            With = lib(Load,Cxt,PackDefs),
            catch(lib(Load,Cxt,PackDefs),_,Rep1)
    ),
    call( Cxt:Goal ).

lib_r_promised( Function ) :-
     atom_string( Function, Fstring ),
     real:r_call( exists(Fstring), [rvar(Rex)] ),
     Rex == true,
     real:r_call( 'is.function'(Function), [rvar(Rfu)] ),
     Rfu == true,
     !.
lib_r_promised( Function ) :-
     ( lib_tables:lib_promise_r(Function,Library) ->
          real:r_library(Library)
          ;
          throw( dont_know_what_to_load_for_r_promised(Function) )
     ).
