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
@version  0.2 2026/1/8    more efficient implementation

*/
has_at_least( N, X, List ) :-
	ground( X ),
     integer( N ),
     N > 0,
     has_at_least_1( N, List, X ).

has_at_least_1( 0, _List, _X ) :-
     !.
has_at_least_1( N, [H|T], X ) :-
     ( H == X ->
          M is N - 1
          ;
          M is N
     ),
     has_at_least_1( M, T, X ).
