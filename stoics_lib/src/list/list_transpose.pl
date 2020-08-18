:- use_module(library(apply)).  % maplist/2.
:- lib( skim/3 ).

/** list_transpose( +List, -Transpose ).

Transpose a list of lists.

==
?- list_transpose( [[a,1,2,3],[b,4,5,6],[c,7,8,9]], Trans ).
Trans = [[a, b, c], [1, 4, 7], [2, 5, 8], [3, 6, 9]].
==

@author nicos angelopoulos
@version  0.1 2017/1/11

*/

list_transpose( Nest, Trans ) :-
	skim( Nest, H, Rest ),
	!,
	Trans = [H|Tail],
	list_transpose( Rest, Tail ).
list_transpose( Empties, [] ) :-
	maplist( ==([]), Empties ).
