%% options_return( +TermS, +OptS ).
%
% The predicate always succeeds. In addition, if memberchk( Term, OptS )
% succeeds of single Term in the possibly listed term list TermS,
% then any unbound parts of TermS maybe become more ground.
%
% This predicate can be used to instantiate return options.
%
% OptS and TermS are passed through en_list/2.
%
%==
% ?- options_return( cnm(Cnm), [abc(x),cnm(symbols)] ).
% Cnm = symbols.
% ?- options_return( [data(Dt),cnm(Cnm)], [abc(x),cnm(symbols)] ).
% Cnm = symbols
%==
%
options_return( TermS, OptS ) :-
	options_en_list( OptS, Opts ),
	options_en_list( TermS, Terms ),
	options_return_lists( Terms, Opts ).

options_return_lists( [], _Opts ).
options_return_lists( [Term|T], Opts ) :-
	( memberchk( Term, Opts ) -> true; true ),
	options_return_lists( T, Opts ).
