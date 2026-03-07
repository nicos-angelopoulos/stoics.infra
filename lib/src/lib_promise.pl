/** lib_promise( +PidS, +Cxt, +Load ).

    PidS is a predicate id, or list of predicate ids promised in module context Cxt and to be loaded with Load at run time.

    No code is loaded here, until the first call to one of the Pids. 
    Cxt is the context in which the code will be loaded.

    Load can be the name of an installed library (which also supports packs). Or, r(RFunction) for loading _R_ code. 
    Or, _call(Goal)_ that is called as =|Cxt:call(Goal)|=.

    The predicate creates the infrastructure of table entries and stub code that on run time triggeres the loading.

==
:- lib(promise(testo/2,pesto/0,real)).

% Loads system library real, when testo/2 is executed which contains a reference to undefined pesto/0.

:- lib(promise(resto/2,nesto/0,r(pheatmap))).

% Loads R library pheatmap, when resto/2 is called which contains a reference to undefined nesto/0.

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
lib_promise_pid( _Load, Cxt, Promised ) :-
    current_predicate( Cxt:Promised ),
    !.
lib_promise_pid( Load, _Cxt, Promised ) :-
    current_predicate( _:Promised ),
    Load \= call(_),
    lib( Load ),
    !.  % fixme: Load:Promised would be ideal?
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

/** lib_load_promised( +Load, +Cxt, +ReqBy +Goal ).

Fullfills the loading of promised predicate and calls goal.

This is the internal predicate called from the dynamically asserted, holding, hot-swapped code.

The predicate will only call each unique Cxt:Gid, where Gid is Goal's PredID, once.

file test_promise.pl 
==

:- lib(promise(en_list/2,stoics_lib)).

testo :-
    write( start ), nl,
    % can add, the following which will provide the caller in case of error
    % lib_promised( en_list/2, user, testo/0 ),
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
    Feport = lib_message_report(Mess, [With], informational ),
    ( Load = call(_) -> 
            With = Cxt:Load
            ; 
            lib_defaults(pack,PackDefs),
            With = lib(Load,Cxt,[mode(promised)|PackDefs])
    ),
    ( catch(With,_,fail) -> 
          call( Cxt:Goal )
          ;
          call( Feport )
    ).

lib_promised( Pid, Cxt, _ReqBy ) :-
     \+ lib_tables:lib_promise(_L,Cxt:Pid),
     current_predicate( Cxt:Pid ),
     !.
lib_promised( Pid, Cxt, ReqBy ) :-
     findall( Load, lib_tables:lib_promise(Load,Cxt:Pid), LoadS ),
     sort( LoadS, Loads ),
     ( Loads = [Load] -> true; throw( non_unique_load_point_for(Loads,Cxt:Pid,req_by(ReqBy)) ) ),
    forall( lib_tables:lib_promise(Load,Cxt:Pid),
                    (
                        retract(lib_tables:lib_promise(Load,Cxt:Pid)),
                        abolish(Cxt:Pid)
                    )
          ),
    ( Load = call(_) -> 
            With = Cxt:Load
            ; 
            lib_defaults(pack,PackDefs),
            With = lib(Load,Cxt,[mode(promised)|PackDefs])
    ),
    ( catch(With,_,fail) -> 
          ( current_predicate(Cxt:Pid) -> 
               true
               ;
               Mess = 'Loading of: ~w, failed to load code for: ~w required by: ~w.',
               lib_message_report(Mess, [Load,Cxt:Pid,ReqBy], informational )
          )
          ;
          Mess = 'Loading of: ~w, failed. In attempt to define: ~w required by: ~w.',
          lib_message_report(Mess, [Load,Cxt:Pid,ReqBy], informational )
     ).

lib_r_promised( Function, _ReqBy ) :-
     lib_r_function_exists( Function ),
     !.

lib_r_promised( Function, ReqBy ) :-
     ( lib_tables:lib_promise_r(Function,Library) ->
          ( ReqBy == false -> 
               Mess1 = 'Function: ~w, failed to load from: ~w.',
               Mess2 = 'Promised function: ~w. Failed to load source: ~w.',
               Margs = [Function,Library]
               ;
               Mess1 = 'Required by: ~w, promised function: ~w, failed to load from source: ~w.',
               Mess2 = 'Required by: ~w, promised function: ~w. Failed to load source: ~w.',
               Margs = [ReqBy,Function,Library]
          ),
          ( catch(real:r_library(Library), _, fail) ->
               ( lib_r_function_exists(Function) ->
                         true
                         ;
                         Rep = lib_message_report(Mess1, Margs, informational),
                         call( Rep )
               )
               ;
               call( Rep )
          ),
          Rep = lib_message_report(Mess2, Margs, informational)
          ;
          throw( dont_know_what_to_load_for_r_promised(Function) )
     ).

lib_r_function_exists( Function ) :-
     atom_string( Function, Fstring ),
     real:r_call( exists(Fstring), [rvar(Rex)] ),
     Rex == true,
     real:r_call( 'is.function'(Function), [rvar(Rfu)] ),
     Rfu == true.
