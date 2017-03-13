/** nth1( ?N, +List, ?With, ?Nth, +NewList ).

Find and replace the N-th element of a List. 
The list with the element replaced is in NewList.
Nth is the old value and With is the new one.

==
?- nth1( 3, [a,b,c,d], 3, What, New ).
What = c,
New = [a, b, 3, d].
==

@author Nicos Angelopoulos
@version 0.2 2011/?/?, 2005/02/23.
@version 0.3 2017/3/13   renamed from nth_replace/5

*/
nth1( I, List, With, Nth, NewList ) :-
     number(I),
     !,
     nth1_n( I, List, With, Nth, NewList ).
nth1( I, List, With, Nth, NewList ) :-
     var( I ),
     nth1_gen( List, With, Nth, 1, I, NewList ).

nth1_gen( [H|T], With, H, I, N, NewList ) :-
     N is I,
     NewList = [With|T].
nth1_gen( [H|T], With, Nth, I, N, [H|R] ) :-
     NxI is I + 1,
     nth1_gen( T, With, Nth, NxI, N, R ).

nth1_n( 1, List, With, Nth, NewList ) :-
	!,
	List = [Nth|T],
	NewList = [With|T].
nth1_n( N, [H|T], With, Nth, [H|NewT] ) :-
	NxN is N - 1,
	nth1_n( NxN, T, With, Nth, NewT ).
