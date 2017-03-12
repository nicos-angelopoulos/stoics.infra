
/** options_debug_topic( +Topic, +Opts, -Restore ).

Start debugging Topic iff options(debug(true),Opts), with Restore
being instantiated to a term that can be used to restore the 
original debug state of Topic (see options_restore/2).

==
?- options_debug_topic( ex1, [debug(false)], Restore ).
Restore = [].

?- options_debug_topic( ex1, [debug(true)], Restore ).
Restore = ['$restore'(ex1, debug, false)].

==

@author nicos angelopoulos
@version  0.1 2016/8/22

*/
options_debug_topic( Topic, Options, Restore ) :-
	options( debug(true), Options ),
	!,
	options_debug_topic_previously( Topic, Restore ),
	debug( Topic ).
options_debug_topic( _Topic, _Opts, [] ).

options_debug_topic_previously( Topic, Restore ) :- 
	Dbging =.. [debugging,Topic],
	% debugging( Topic ),
	call( Dbging ),
	!,
	Restore = ['$restore'(Topic,debug,true)].
options_debug_topic_previously( Topic, ['$restore'(Topic,debug,false)] ).
