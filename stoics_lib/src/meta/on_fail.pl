
:- lib(suggests(options)).

:- lib(mod_goal/2).
:- lib(mod_goal/3).
:- lib(message_report/3).

on_fail_defaults( Defs  ) :-
    Types = [  mtype-oneof([error,informational,warning]), % fixme: find those from somewhere ?
               rep-oneof([all,both,exception,failure,false,true]),
               rethrow-boolean
            ],
    Defs = [   
                catch(_Catcher),
                mtype(informational),
                rep(exception),
                rethrow(true),
                options_types(Types)
            ].

/** on_fail( +Goal, +Call ).
    on_fail( +Goal, +Call, +Opts ).

    If Goal fails or exceptions (where exception is catched by Catcher, see Opts),
    then Call is called. The predicate in these cases might report the incident
    on the std output depending on the value of option rep(Rep).

    Currently the predicate does not protect the call to Call.
    This is likely to change.

Opts 
  * catch(Catcher)
     free var by default (catches everything) user can pass something more specific
  * mtype(Mtype=informational)
     type of message, also: warning or error (see message_report/3)
  * rep(Rep=exception)
     alternatively: failure, true/both/all or none/false
  * rethrow(Rethrow=true)
     whether to rethrow the exception (after calling Call).

==
?- on_fail( none, true ).
% While calling: none/0, caught exception: error(existence_error(procedure,stoics_lib:none/0),context(system:catch/3,_1530)), now calling: true/0
ERROR ...
...

?- on_fail( none, true, rethrow(false) ).
% While calling: none/0, caught exception: error(existence_error(procedure,stoics_lib:none/0),context(system:catch/3,_4114)), now calling: true/0
true.

?- on_fail( none, true, [rep(false),rethrow(false)] ).
true

?- on_fail( none, true, [rep(exception),rethrow(false)] ).
% While calling: none/0, caught exception: error(existence_error(procedure,stoics_lib:none/0),context(system:catch/3,_9454)), now calling: true/0
true.

?- on_fail( fail, true, [rep(exception),rethrow(false)] ).
true.

?- on_fail( fail, true, rep(both)  ).
% Call to fail/0, failed, calling: true/0
true.
==

@author nicos angelopoulos
@version  0.1 2017/08/11, lil'B

*/
on_fail( Goal, Call ) :-
    on_fail( Goal, Call, [] ).

on_fail( Goal, Call, Args ) :-
    options_append( on_fail, Args, Opts ),
    options( [catch(Catcher),mtype(Mtype),rep(Rep),rethrow(Reth)], Opts ),
    on_fail_catch( Goal, Catcher, Rep, Mtype, Reth, Call ).

on_fail_catch( Goal, Catcher, Rep, Mtype, Reth, Call ) :-
    on_fail_reports_caught( Rep, RepB ),
    mod_goal( Goal, Moal ),             % Moal Mod:Goal form
    mod_goal( _Mod, Noal, Moal ),       % Noal Goal guaranteed with no Mod prepending form
    functor( Noal, Gnm, Gar ),
    catch( Moal, Catcher, on_fail_caught(RepB,Mtype,Gnm/Gar,Catcher,Reth,Call) ),
    !.
on_fail_catch( Goal, _Catcher, Rep, Mtype, _Reth, Call ) :-
    on_fail_reports_failure( Rep, RepB ),
    mod_goal( Mod, Noal, Goal ),
    functor( Noal, Gnm, Gar ),
    on_fail_failed( RepB, Mtype, Mod:Gnm/Gar, Call ).

on_fail_caught( true, Mtype, Gid, Catcher, Reth, Call ) :-
    functor( Call, Cnm, Car ),
    Mess = 'While calling: ~w, caught exception: ~w, now calling: ~w',
    message_report( Mess, [Gid,Catcher,Cnm/Car], Mtype ),
    call(Call),
    on_fail_rethrow( Reth, Catcher ).
on_fail_caught( false, _Mtype, _Gid, Catcher, Reth, Call ) :-
    call(Call),
    on_fail_rethrow( Reth, Catcher ).

on_fail_rethrow( true, Catcher ) :-
    throw( Catcher ).
on_fail_rethrow( false, _Catcher ).

on_fail_failed( true, Mtype, Gid, Call ) :-
    mod_goal( Call, Mall ),
    mod_goal( Mod, Nall, Mall ),
    functor( Nall, Cnm, Car ),
    Mess = 'Call to ~w, failed, calling: ~w',
    message_report( Mess, [Gid,Mod:Cnm/Car], Mtype ),
    call( Mall ).
on_fail_failed( false, _Mtype, _Gid, Call ) :-
    mod_goal( Call, Mall ),
    call( Mall ).

on_fail_reports_failure(false, false).
on_fail_reports_failure(failure, true).
on_fail_reports_failure(exception, false).
on_fail_reports_failure(true, true).
on_fail_reports_failure(all, true).
on_fail_reports_failure(both, true).

on_fail_reports_caught(false, false).
on_fail_reports_caught(failure, false).
on_fail_reports_caught(exception, true).
on_fail_reports_caught(true, true).
on_fail_reports_caught(all, true).
on_fail_reports_caught(both, true).
