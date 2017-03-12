%% options_debug( +Format, +Args, +Opts ).
%
% Call debug(_,Format,Args) iff debug(true) is the first debug(_) term in 
% list Opts
%==
% ?- options_debug( 'A simple message at: ~w', noon, [debug(true)] ).
% ?- options_debug( 'A simple message at: ~w', noon, [] ).
%==
%
% @author nicos angelopoulos
% @version  0.1 2015/1/15
%
options_debug( Format, Args, Opts ) :-
	memberchk( debug(Dbg), Opts ),
	!,
	ground( Dbg ), % fixme: report rather than just failing...
	option_boolean_debug( Dbg, Format, Args ).
options_debug( _Format, _Args, _Opts ).

% should we allow debug reporting for self here ?
option_boolean_debug( true, Format, Args ) :-
	!,
	Dbg =.. [debug,_,Format,Args],
	call( Dbg ).
	% debug( _, Format, Args ).
option_boolean_debug( _, _Format, _Args ).
