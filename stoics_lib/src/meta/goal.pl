
:- lib(en_list/2).

/** goal( +Partial, +ArgS, +Mod, -Goal ).

Construct Goal from a partial or predicate name either of which might be moded
and some arguments. If none of these is moded, Mod is used. 

==
?- goal( p, x, u, G ).
G = u:p(x).

?- goal( a:p(t), x, u, G ).
G = a:p(t, x).

?- goal( a:b:p, x, u, G ).
false.

==

@author nicos angelopoulos
@version  0.1 2015/3/30

*/
goal( Mod:Partial, Args, _Def, Goal ) :-
	!,
	Partial \= (_:_),
	goal( Partial, Args, Mod, Goal ).
goal( Partial, ExtraArgS, Mod, Goal ) :-
	compound( Partial ),
	!,
	Partial =.. [PredName|PartArgs],
	en_list( ExtraArgS, ExtraArgs ),
	append( PartArgs, ExtraArgs, Args ),
	Full =.. [PredName|Args],
	Goal = (Mod:Full).
goal( PredName, ArgS, Mod, Goal ) :-
	en_list( ArgS, Args ),
	Full =.. [PredName|Args],
	Goal = (Mod:Full).
