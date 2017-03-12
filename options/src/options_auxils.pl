options_auxils.

options_message_report( Format, Args, Kind ) :-
	phrase('$messages':translate_message(debug(Format,Args)), Lines),
	print_message_lines(current_output, kind(Kind), Lines).

options_compound( Term, Name, Args ) :-
	current_predicate( compound_name_arguments/3 ),
	!,
	once( (compound(Term) ; (ground(Name),is_list(Args))) ),
	% !,
	compound_name_arguments( Term, Name, Args ).
options_compound( Term, Name, Args ) :-
	once( (compound(Term) ; (ground(Name),ground(Args))) ),
	Term =.. [Name,Args].

options_en_list( In, Opts ) :-
	is_list( In ),
	!,
	Opts = In.
options_en_list( In, Opts ) :-
	\+ var( In ),
	!,
	Opts = [In].
options_en_list( Else, _Opts ) :-
	throw( options_en_list(2,encountered_variable_in_1st_arg(Else)) ).
