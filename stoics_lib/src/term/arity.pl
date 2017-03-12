/** arity( ?Term, ?Name, ?Arity ).
    arity( ?Term, ?Arity ).

This is the permissive version, if we detect atomic
we use functor/3 (the old stuff), otherwise we call 
compound_name_arity/3.

@author nicos angelopoulos 
@version  0.1 2014/1/10

*/
arity( Term, Arity ) :-
	arity( Term, _Name, Arity ).

arity( Term, Name, Arity ) :-
	current_predicate( compound_name_arity/3 ),
	\+ atomic( Term ),
	!,
	compound_name_arity( Term, Name, Arity ).
arity( Term, Name, Arity ) :-
	functor( Term, Name, Arity ).
