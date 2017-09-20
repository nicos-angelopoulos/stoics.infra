:- lib(mod_goal/4).

/** map_list_options( +Goal, ?InList, ?OutList, +Opts ).
    map_list_options( +Goal, ?InList, +Opts ).

Very simple implementation of maplist/2,3 + Options. 
Goal is called in user if not module prepended (mod_goal/4).

==
?- assert( (ex_mlo(No,Out,Opts) :- Out is No + 1, write( opts(Opts) ), nl) ).
?- map_list_options( ex_mlo, [1,2,3], Outs, [a(b),b(c)] ).
opts([a(b),b(c)])
opts([a(b),b(c)])
opts([a(b),b(c)])
Outs = [2, 3, 4]
==

@author nicos angelopoulos
@version  0.1 2016/5/23
@version  0.2 2017/9/20,  moved to stoics_lib, added example, pass Goal through mod_goal/4

*/
map_list_options( GoalIn, InL, Opts ) :-
    mod_goal( user, GoalIn, false, Goal ),
	map_list_options_3( InL, Goal, Opts ).

map_list_options( GoalIn, InL, OutL, Opts ) :-
	var( InL ),
	ground( OutL ),
	!,
    mod_goal( user, GoalIn, false, Goal ),
	maplist_options_2( OutL, Goal, InL, Opts ).
map_list_options( GoalIn, InL, OutL, Opts ) :-
    mod_goal( user, GoalIn, false, Goal ),
	maplist_options_1( InL, Goal, OutL, Opts ).

maplist_options_1( [], _Goal, [], _Opts ).
maplist_options_1( [InH|InT], Goal, [OutH|OutT], Opts ) :-
	call( Goal, InH, OutH, Opts ),
	maplist_options_1( InT, Goal, OutT, Opts ).

maplist_options_2( [], _Goal, [], _Opts ).
maplist_options_2( [OutH|OutT], Goal, [InH|InT], Opts ) :-
	call( Goal, InH, OutH, Opts ),
	maplist_options_2( OutT, Goal, InT, Opts ).

map_list_options_3( [], _Goal, _Opts ).
map_list_options_3( [H|T], Goal, Opts ) :-
	call( Goal, H, Opts ),
	map_list_options_3( T, Goal, Opts ).
