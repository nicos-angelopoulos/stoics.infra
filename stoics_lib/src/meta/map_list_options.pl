
:- lib(en_list/2).
:- lib(mod_goal/4).

map_list_options_defaults( Defs ) :-
    Defs = [
                add_options(true),
                call_options([]),
                failed(_),
                on_fail(skip),
                options_types([on_fail-oneof([skip,fail]),add_options-boolean])
    ].

/** map_list_options( +Goal, ?InList, ?OutList, +Opts ).
    map_list_options( +Goal, ?InList, +Opts ).

An implementation of maplist/2,3 with Options. 
It addition to allowing Options to be passed to the map_list Goal, it also
presents a common interface for maplist/2,3 and map_succ_list/3,4.

The predicate introduces the concept of direction. Are we generating InList from OutList or
OutList from InList ? Currently this is done automatically and only affects Failed (see options).
The direction might become more explicit with a new option (_auto_, _left_ and _right_).
Currently direction is _right_ (generating InList from OutList) if Outlist is ground and InList is not,
and _left_ otherwise.

Opts 
 * add_options(AddOpts=true)
 false if do not wish to add Copts to call to Goal (AddOpts is passed through en_list/2)
 * call_options(Copts)
 Options to pass to Goal (as last argument)
 * failed(Failed)
 returns the list of failed elements (direction dependent)
 * on_fail(OnFail=skip)
 If OnFail is _fail_, the whole predicate fails if Goal fails, _error_ throws a ball

==
?- assert( (ex_mlo(No,Out,Opts) :- Out is No + 1, write( opts(Opts) ), nl) ).
?- map_list_options( ex_mlo, [1,2,3], Outs, call_options([a(b),b(c)]) ).
opts([a(b),b(c)])
opts([a(b),b(c)])
opts([a(b),b(c)])
Outs = [2, 3, 4]

?- assert( (plus_one(A,B) :- (var(A) -> B < 5, A is B - 1; A < 5, B is A + 1)) ).
true.

?- map_list_options( plus_one, [1,2,3], Out, [] ).
ERROR: Undefined procedure: plus_one/3
ERROR:   However, there are definitions for:
ERROR:         plus_one/2
...

?- map_list_options( plus_one, In, [2,3,4], add_options(false) ).
In = [1, 2, 3].

?- map_list_options( plus_one, In, [2,3,4,5], [add_options(false),on_fail(error)] ).
ERROR: Unhandled exception: failure_on_map_list_options_call(user:plus_one,_15236,5)
==

Emulate maplist/2,3 
==
?- map_list_options( plus_one, [1,2,3,4,5], Out, [add_options(false),on_fail(fail)] ).
false.

?- map_list_options( plus_one, [1,2,3,4], Out, [add_options(false),on_fail(fail)] ).
Out = [2, 3, 4, 5].
==

Emulate map_succ_list/3,4 
==
?- map_list_options( plus_one, [1,2,3,4,5], Out, [add_options(false),failed(Failures)] ).
Out = [2, 3, 4, 5],
Failures = [5].

?- map_list_options( plus_one, In, [1,2,3,4,6], [add_options(false),failed(Failures)] ).

?- map_succ_list( plus_one, In, [1,2,3,4,6], Rej ).
In = [0, 1, 2, 3],
Rej = [6].
==

@author nicos angelopoulos
@version  0.1 2016/5/23
@version  0.2 2017/9/20,  moved to stoics_lib, added example, pass Goal through mod_goal/4
@version  0.3 2019/2/25,  added error on failure, add_options(AddOpts), on_fail(OnFail),  and moved call options to option call_options(Copts).
@tbd      pretty print the error message.

*/
map_list_options( GoalIn, InL, Args ) :-
    options_append( map_list_options, Args, Opts ),
    mod_goal( user, GoalIn, Goal, override(false) ),
    options( add_options(AddOpts), Opts ),
    options( call_options(COptsPrv), Opts ),
    en_list( COptsPrv, COpts ),
    options( on_fail(OnFail), Opts ),
    map_list_options_single( InL, AddOpts, COpts, OnFail, Goal, Failed ),
    options( failed(Failed), Opts ).

map_list_options( GoalIn, InL, OutL, Args ) :-
    options_append( map_list_options, Args, Opts ),
    mod_goal( user, GoalIn, Goal, override(false) ),
    options( add_options(AddOpts), Opts ),
    options( call_options(COpts), Opts ),
    options( on_fail(OnFail), Opts ),
    map_list_options( Goal, InL, OutL, AddOpts, COpts, OnFail, Failed ),
    options( failed(Failed), Opts ).

map_list_options( Goal, InL, OutL, AddOpts, COpts, OnFail, Failed ) :-
    \+ ground(InL),
    % var( InL ),
    ground( OutL ),
    !,
    maplist_options_compound_right( OutL, Goal, AddOpts, COpts, OnFail, InL, Failed ).
map_list_options( Goal, InL, OutL, AddOpts, COpts, OnFail, Failed ) :-
    maplist_options_compound_left( InL, Goal, AddOpts, COpts, OnFail, OutL, Failed ).

maplist_options_compound_left( [], _Goal, _AddOpts, _COpts, _OnFail, [], [] ).
maplist_options_compound_left( [InH|InT], Goal, AddOpts, COpts, OnFail, Out, Failed ) :-
    maplist_options_call( AddOpts, COpts, Goal, InH, _OutH, OnFail, left, Out, Failed, OutT, Tailed ),
    maplist_options_compound_left( InT, Goal, AddOpts, COpts, OnFail, OutT, Tailed ).

maplist_options_compound_right( [], _Goal, _AddOpts, _COpts, _OnFail, [], [] ).
maplist_options_compound_right( [OutH|OutT], Goal, AddOpts, COpts, OnFail, In, Failed ) :-
    maplist_options_call( AddOpts, COpts, Goal, _InH, OutH, OnFail, right, In, Failed, InT, Tailed ),
    maplist_options_compound_right( OutT, Goal, AddOpts, COpts, OnFail, InT, Tailed ).

maplist_options_call( false, _COpts, Goal, InElem, OutElem, OnFail, Dir, List, Failed, Tail, Tailed ) :-
    maplist_options_call_without( Goal, InElem, OutElem, OnFail, Dir, List, Failed, Tail, Tailed ).
maplist_options_call( true, COpts, Goal, InElem, OutElem, OnFail, Dir, List, Failed, Tail, Tailed ) :-
    maplist_options_call_with( Goal, COpts, InElem, OutElem, OnFail, Dir, List, Failed, Tail, Tailed ).

maplist_options_call_without( Goal, InElem, OutElem, _OnFail, Dir, List, Failed, Tail, Tailed ) :-
    call( Goal, InElem, OutElem ),
    !,
    maplist_options_success( Dir, InElem, OutElem, List, Tail ),
    Tailed = Failed.
maplist_options_call_without( Goal, InElem, OutElem, OnFail, Dir, List, Failed, Tail, Tailed ) :-
    ( OnFail == error ->
        throw(failure_on_map_list_options_call(Goal,InElem,OutElem) )
        ;
        ( OnFail == skip ->
            List = Tail,
            maplist_options_dir_element( Dir, InElem, OutElem, Elem ),
            Failed = [Elem|Tailed]
            ;
            fail
        )
    ).

maplist_options_dir_element( left, InElem, _OutElem, InElem ).
maplist_options_dir_element( right, _InElem, OutElem, OutElem ).

maplist_options_call_with( Goal, COpts, InElem, OutElem, _OnFail, Dir, List, Failed, Tail, Tailed ) :-
    call( Goal, InElem, OutElem, COpts ),
    !,
    maplist_options_success( Dir, InElem, OutElem, List, Tail ),
    Tailed = Failed.
maplist_options_call_with( Goal, COpts, InElem, OutElem, OnFail, Dir, List, Failed, Tail, Tailed ) :-
    ( OnFail == error ->
        throw(failure_on_map_list_options_call(Goal,InElem,OutElem,COpts) )
        ;
        ( OnFail == skip ->
            List = Tail,
            maplist_options_dir_element( Dir, InElem, OutElem, Elem ),
            Failed = [Elem|Tailed]
            ;
            fail
        )
    ).

maplist_options_success( left, _InElem, OutElem, [OutElem|Tail], Tail ).
maplist_options_success( right, InElem, _OutElem, [InElem|Tail], Tail ).

map_list_options_single( [], _AddOpts, _COpts, _OnFail, _Goal, [] ).
map_list_options_single( [H|T], AddOpts, COpts, OnFail, Goal, Failed ) :-
    map_list_options_single_elem( AddOpts, COpts, OnFail, Goal, H, Failed, Tailed ),
    map_list_options_single( T, AddOpts, COpts, OnFail, Goal, Tailed ).

map_list_options_single_elem( true, COpts, OnFail, Goal, Elem, Failed, Tailed ) :-
    map_list_options_single_elem_with( OnFail, Goal, Elem, COpts, Failed, Tailed ).
map_list_options_single_elem( false, _COpts, OnFail, Goal, Elem, Failed, Tailed ) :-
    map_list_options_single_elem_without( OnFail, Goal, Elem, Failed, Tailed ).

map_list_options_single_elem_without( skip, Goal, Elem, Failed, Tailed ) :-
    ( call(Goal,Elem) -> Failed = Tailed ; Failed = [Elem|Tailed] ).
map_list_options_single_elem_without( fail, Goal, Elem, Failed, Tailed ) :-
    call( Goal, Elem ),
    Failed = Tailed.
map_list_options_single_elem_without( error, Goal, Elem, Failed, Tailed ) :-
    ( call(Goal,Elem) -> Failed=Tailed; throw(failure_on_maplist_call(Goal,Elem)) ).

map_list_options_single_elem_with( skip, Goal, Elem, COpts, Failed, Tailed ) :-
    ( call(Goal,Elem,COpts) -> Failed = Tailed ; Failed = [Elem|Tailed] ).
map_list_options_single_elem_with( fail, Goal, Elem, COpts, Failed, Tailed ) :-
    call( Goal, Elem, COpts ),
    Failed = Tailed.
map_list_options_single_elem_with( fail, Goal, Elem, COpts, Failed, Tailed ) :-
    ( call(Goal,Elem,COpts) -> Failed = Tailed ; throw(failure_on_maplist_call(Goal,Elem)) ).
