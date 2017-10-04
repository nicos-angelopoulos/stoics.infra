:- lib( pack_errors ).

%% termplate( +Term, ?Arity, -Termplate ).
%% termplate( +Term, -Termplate ).
%
% Termplate has the same Arity and functor as Term, but all 
% its arguments are unbound variables. Version 0.2 works for lists and atoms too.
%
%==
% ?- termplate( t(a,b,c), Arity, Template ).
% Arity = 3,
% Template = t(_G6305, _G6306, _G6307).
%
% ?- termplate( [a,b,c], Arity, Template ).
% Arity = 3,
% Template = [_8920, _8926, _8932].
% 
% ?- termplate( a, Arity, Template ).
% Arity = 0,
% Template = a.
% 
% ?- termplate( A, Arity, Template ).
% ERROR: Arguments are not sufficiently instantiated
% ...
%==
%
% @author nicos angelopoulos
% @version  0.1 2016/12/11
% @version  0.2 2017/10/04,  allow Term to be a list or an atom, error handling for var Term
%
termplate( Term, Termplate ) :-
    termplate( Term, _Arity, Termplate ).

termplate( Term, Arity, Termplate ) :-
     must_be( nonvar, Term ),
     termplate_nonvar( Term, Arity, Termplate ).

termplate_nonvar( Term, Arity, Termplate ) :-
    is_list( Term ),
    !,
    length( Term, Arity ),
    length( Termplate, Arity ).
termplate_nonvar( Term, Arity, Termplate ) :-
    atomic( Term ),
    !,
    Arity = 0,
    Term = Termplate.
termplate_nonvar( Term, Arity, Termplate ) :-
    functor( Term, Name, Arity ),
    functor( Termplate, Name, Arity ).
