/** has_at_least( +N, +X, +List ).

	Succeeds iff List contains at least N Xs.

==
?- has_at_least( 2, a, [a,b,c,a] ).
true.

?- has_at_least( 2, b, [a,b,c,a] ).
false.
==

@author nicos angelopoulos
@version  0.1 2017/1/11

*/
has_at_least( N, X, List ) :-
	ground( X ),
	findall( 1, member(X,List), Ones ),
	length( Ones, Len ),
	N =< Len.
