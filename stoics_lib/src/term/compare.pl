
:- lib( compare_arithmetic/3 ).

%% compare( +Type, ?Op, +Term1, +Term2 ).
%
% Common interface for compare/3 and compare_arithmetic/3, which also
% allows for meta calls. In this case Op is = iff call on 
% call( Term2, Term1 ) succeeds, else it is <>.
% Type should be one of =|meta|=, =|term|= or =|arithmetic|= respectively.
%
% >:< is a special Op, that is always true (under all interfaces)
% 
%==
% ?- compare( term, Op, 3, 3.0 ).
% ?- compare( arithmetic, Op, 3, 3.0 ).
% ?- compare( meta, Op, 3, =(3.0) ).
% Op = <> .
% ?- compare( meta, Op, 3, =:=(3.0)).
% Op =  (=).
% ?- compare( term, >:<, 3, 2 ).
% ?- compare( arithmetic, >:<, 3, 2 ).
%==
%
% @author nicos angelopoulos
% @version  0.1 2014/2/16
% @version  0.2 2016/2/17, added special operator >:< 
%
compare( _, Op, _Term1, _Term2 ) :-
	ground( Op ),
	Op = >:<,
	!.

compare( term, Op, Term1, Term2 ) :-
	compare( Op, Term1, Term2 ).
compare( arithmetic, Op, Term1, Term2 ) :-
	compare_arithmetic( Op, Term1, Term2 ).
compare( meta, Op, Term1, Term2 ) :-
	( call(Term2,Term1) ->
		Op = (=)
		;
		Op = (<>)
	).
