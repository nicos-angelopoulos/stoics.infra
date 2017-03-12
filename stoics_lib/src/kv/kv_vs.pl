/** kv_vs( +KVs, -Vs ).

Vs are all values in the key values, -pairs, KVs. 
0.2 supports any /n terms as KVs by means of using arg/3.

==
?- kv_vs( [a-1,b-2,c-3], Vs ).
Vs = [1, 2, 3].

?- kv_vs( [t(1,a,'A'),t(2,b,'B'),t(3,c,'C')], Vs ).
Vs = [a, b, c].
==

@author nicos angelopoulos
@version  0.2 2017/3/12,   use arg/3 rather than argument unification

*/
kv_vs( [], [] ).
kv_vs( [H|T], [V|R] ) :-
    arg( 2, H, V ),
     kv_vs( T, R ).
