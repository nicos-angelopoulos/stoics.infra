/** kv_decompose( +Pairs, -Ks, -Vs ).

    Split -pair list, Pairs, to its K and V lists.

==
?- kv_decompose( [a-1,b-2,c-3], Ks, Vs ).
Ks = [a, b, c],
Vs = [1, 2, 3].
==

@author nicos angelopoulos

*/
kv_decompose( [], [], [] ).
kv_decompose( [K-V|T], [K|Tk], [V|Tv] ) :-
	kv_decompose( T, Tk, Tv ).
