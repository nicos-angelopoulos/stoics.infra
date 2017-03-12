
options_call_defaults( call_module(user) ).

/** options_call( +Opt, +Goal, +Opts ).

Call Goal iff =options(Opt,Opts)= succeeds.

Opts can in addition to Opt include
 * call_module(Mod=user)
   which defines the calling module if one is not in Goal
 
==
?- options_call( write(true), (write(true),nl), write(true) ).
true
true.
?- options_call( write(flase), (write(true),nl), write(false) ).
true.
?- options_call( write(true), user:(write(true),nl), write(true) ).
true
true.
==

@author nicos angelopoulos
@version  0.1 2017/2/15

*/
options_call( Opt, Goal, Args ) :-
	options_append( options_call, Args, Opts ),
    options( Opt, Opts ),
    options( call_module(Mod), Opts ),
    !,
    call( Mod:Goal ).
options_call( _Opt, _Goal, _OptS ).

/** options_call( +Goal, +Opts ).

Call Goal by extending it with a first argument that comes from
selecting a homonymous option from Opts.

Opts 
 * call_module(Mod=user)  
   the module in which to call extended Goal
 
==
?- options_call( plus(2,X), plus(1) ).
X = 3
==

@author nicos angelopoulos
@version  0.1 2015/12/15
@ allow for , separate goals ? 

*/
options_call( Goal, Args ) :-
	options_append( options_call, Args, Opts ),
	options_compound( Goal, Pname, Grgs ),
	options_compound(  Opt, Pname, [Org] ),
	options( Opt, Opts ),
	% it is safe to use =.. now as we have at 1 arg
	Callable =.. [Pname,Org|Grgs],
	options( call_module(Mod), Opts ),
	call( Mod:Callable ).
