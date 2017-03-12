/** os_succ( +Goal, +Error, +Opts ).

Call Goal with error capture as failure.
On failure, throws packaged Error iff the value
of option report(Report) is Report==error.
Option report()  should exist. The only alternative
value is Report==fail. 

@tbd this should go to pack(pack_error).

*/
os_succ( Goal, Error, Opts ) :-
	catch( Goal, _, fail ),
	!.
os_succ( _Goal, Error, Opts ) :-
	options( report(error), Opts ),
	!,
	Error=e(Pname,Arity,ETerm),
	throw( pack_error(os,Pname/Arity,ETerm) ).
os_succ( _Goal, _Error, Opts ) :-
	options( report(fail), Opts ),
	!.
os_succ( _Goal, _Error, Opts ) :-
	% fimxe: 
	Error=e(Pname,Arity,ETerm),
	throw( pack_error(os,Pname/Arity,opt_mismatch(report,[error,fail],Opts) ).
