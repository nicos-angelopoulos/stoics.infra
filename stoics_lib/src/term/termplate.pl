%% termplate( +Term, -Arity, -Termplate ).
%% termplate( +Term, -Termplate ).
%
% Termplate has the same Arity and functor as Term, but all 
% its arguments are unbound variables.
%
%==
% termplate( t(a,b,c), Arity, Template ).
% Arity = 3,
% Template = t(_G6305, _G6306, _G6307).
%==
%
% @author nicos angelopoulos
% @version  0.1 2016/12/11
%
termplate( Term, Termplate ) :-
	termplate( Term, _Arity, Termplate ).

termplate( Term, Arity, Termplate ) :-
     functor( Term, Name, Arity ),
     functor( Termplate, Name, Arity ).
