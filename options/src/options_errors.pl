options_errors.

:- multifile( pack_errors:message/3 ).

pack_errors:message( opt_mustbe_ground(Opt,Opts) ) -->
	['Option should be ground, but found: ~w, in options: ~w'-[Opt,Opts]].

pack_errors:message( opt_required(Opt,Opts) ) -->
	['Required option: ~w, not present in options: ~w' - [Opt,Opts]].

pack_errors:message( opt_mismatch(Opt,Vals,Opts) ) -->
	{ Term =.. [Opt,Val],
	  ( memberchk(Term,Opts) -> true; Val = '$internal_error_at_opt_mismatch' )
	},
	['Option: ~w, expects values in: ~w, but found: ~w in options: ~w' - [Opt,Vals,Val,Opts]].

pack_errors:message( only_use_opt_as_ret ) -->
    ['Only return options for returning results, leave unbound at call time.'-[] ].
