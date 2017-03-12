/** has_at_most( +N, +X, +List ).

	Succeeds iff List contains at most N Xs.

==
?- has_at_most( 1, a, [a,b,c,a] ).
false.

?- has_at_most( 1, b, [a,b,c,a] ).
true.
==

@author nicos angelopoulos
@version  0.1 2017/1/11

*/

has_at_most( N, X, List ) :-
	ground( X ),
	findall( 1, member(X,List), Ones ),
	length( Ones, Len ),
	Len =< N.
