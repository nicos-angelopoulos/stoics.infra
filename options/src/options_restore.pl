/** options_restore( +Self, +Opts ).

Restore all options_append/3, auto processed options.

This predicate is often called at the end of a deterministic 
call that depends on a set of options.

Currently the only option that is restored is
 
  * debug(Dbg)

Internals;
  The predicate looks for any macthing '$restore'(Self,OptName,Status), 
  so it should be extensible to other state-based options processing.

*/

options_restore( Self, Opts ) :-
	maplist( option_restore(Self), Opts ).

option_restore( Self, '$restore'(Self,Process,Status) ) :-
	!,
	option_process_restore( Process, Status, Self ).
option_restore( _Self, _Opt ).

option_process_restore( debug, Status, Self ) :-
	option_process_restore_debug( Status, Self ).

option_process_restore_debug( true, Self ) :-
	debug( Self ).
option_process_restore_debug( false, Self ) :-
	nodebug( Self ).
