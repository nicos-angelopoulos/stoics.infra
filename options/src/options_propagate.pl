
options_propagate_defaults( match(first) ).

/** options_propagate( +Onames, +OptsIn, -OptsOut, +PropOpts ).

Propagate all options with names in Onames from OptsIn to OptsOut.
Onames can be a single un_listed option name (en_list/2).

The predicate does not check arities.

The order in OptsOut follows the order in Onames.

PropOpts control the predicate behaviour
  * match(Match=first)
     duplicates are removed, use match(all) for no removal

@author nicos angelopoulos
@version  0.1 2015/3/19
@tbd type checking of Match via options_append/4.

*/

options_propagate( OnameS, Ins, Outs, Args ) :-
	options_en_list( OnameS, Onames ),
	options_append( options_propagate, Args, Opts ),
	options( match(Match), Opts ),
	options_propagate_list( Onames, Match, Ins, Outs ).

options_propagate_list( [], _Match, _Ins, [] ).
options_propagate_list( [H|T], Match, Ins, Outs ) :-
	options_propagate_match( Match, H, Ins, ToOuts ),
	append( ToOuts, TailOuts, Outs ),
	options_propagate_list( T, Match, Ins, TailOuts ).

options_propagate_match( first, H, Ins, ToOuts ) :-
	( (member(Opt,Ins),functor(Opt,H,_)) ->
		ToOuts = [Opt]
		;
		ToOuts = []
	).
options_propagate_match( all, H, Ins, ToOuts ) :-
	findall( Opt, (member(Opt,Ins),functor(Opt,H,_)), ToOuts ).
