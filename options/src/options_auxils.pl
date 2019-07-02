options_auxils.

options_message_report( Format, Args, Kind ) :-
    print_message( Kind, format(Format,Args) ).

options_compound( Term, Name, Args ) :-
	% current_predicate( compound_name_arguments/3 ),
	% !,
	once( (compound(Term) ; (ground(Name),is_list(Args))) ),
	compound_name_arguments( Term, Name, Args ).
% options_compound( Term, Name, Args ) :- % fixme: delete 19.07.02
	% once( (compound(Term) ; (ground(Name),ground(Args))) ),
	% Term =.. [Name,Args].

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

options_remainder( [], _OptNm, _OptAr, _Val, [] ).
options_remainder( [O|Os], OptNm, OptAr, Val, Remain ) :-
    functor( O, OptNm, OptAr ),
    ( ground(Val) ->
        arg( 1, O, Val )
        ;
        true
    ),
    !,
    options_remainder( Os, OptNm, OptAr, Val, Remain ).
options_remainder( [O|Os], OptNm, OptAr, Val, Remain ) :-
    Remain = [O|Temain],
    options_remainder( Os, OptNm, OptAr, Val, Temain ).
