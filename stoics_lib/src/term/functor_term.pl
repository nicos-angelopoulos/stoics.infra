
:- lib( arity/3 ).

%% functor_term( +Functor, -Term ).
%% functor_term( -Functor, +Term ).
% 
%  Convert between a term and a Pname/Arity functor representation.<br>
%  Can be used to make sure a list of terms is of certain (top) functor.<br>
%  This predicate uses arity/3 rather than functor/3.
%
%==
%  ?- maplist( functor_term((-)/2), [a-b,c-d] ).
%  true.
% ?- maplist( functor_term((-)/2), [a-b,c+d] ).
% false.
%
%  ?- maplist( functor_term(term/0), [term,term()] ).
% true.
%==
%
% @author nicos angelopoulos
% @version  0.1 2014/7/28
% @version  0.2 2018/1/8,   tidy-up doc and added to lib(stoics_lib)
%
functor_term( Pname/Arity, Term ) :-
	arity( Term, Pname, Arity ).
