%
options_return_defaults(on_fail(succ)).

%% options_return( +TermS, +OptS ).
%% options_return( +TermS, +OptS, +ORoptS ).
%
% Use first matching to return values of options, typically after the call of an optionised predicate.
% 
% Opts is a listified OptS, and Terms of TermS.
% The intension is that if Term is in Terms and memberchk(Term, Opts) succeeds, then any unbound parts of
% Term will became ground of at least more ground than at call time.
% If the memberchk/2 above fails, then by default the predicate succeeds. Values of the on_fail(OnFail) 
% option control that. Set to fail, for the predicate to fail and throw for an error to be raised.
% The ball is thrown via throw/2 where ORoptS are passed as second argument, so the error can include
% information about caller and pack it originated. See example below.
%
% This predicate can be used to instantiate return options.
%
% ORoptS
%  * on_fail(OnFail=succ)
%    what to do at failure (also: fail, throw).
%
% OptS, ORoptS and TermS are passed through options_en_list/2.
%
%==
% ?- options_return( cnm(Cnm), [abc(x),cnm(symbols)] ).
% Cnm = symbols.
% ?- options_return( [data(Dt),cnm(Cnm)], [abc(x),cnm(symbols)] ).
% Cnm = symbols
%==
%
%==
% ?-  ORopts = [on_fail(throw),pack(bio_analytics),pred(exp_reac_over/3)],
%     options_return( gid_to(to), [], ORopts ).
% 
%==
%
options_return( TermS, OptS ) :-
     options_return( TermS, OptS, [] ).

options_return( TermS, OptS, Args ) :-
     options_append( options_return, Args, ORopts ),
	options_en_list( OptS, Opts ),
	options_en_list( TermS, Terms ),
	options_return_lists( Terms, Opts ).

options_return_lists( [], _Opts ).
options_return_lists( [Term|T], Opts ) :-
	( memberchk( Term, Opts ) -> true; true ),
	options_return_lists( T, Opts ).
