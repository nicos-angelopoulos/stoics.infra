/** kv_ks( +KVs, -Ks ).

Ks are all keys in the key values KVs. 
0.2 supports any /n terms as KVs by means of using arg/3.

==
?- kv_ks( [a-1,b-2,c-3], Ks ).
Ks = [a, b, c].

?- kv_ks( [t(1,a,'A'),t(2,b,'B'),t(3,c,'C')], Ks ).
Ks = [1, 2, 3].
==

@author nicos angelopoulos
@version  0.2             use arg/3 rather than argument unification
@version  0.3 2017/3/12,  docs

*/
kv_ks( [], [] ).
kv_ks( [H|T], [K|R] ) :-
    arg( 1, H, K ),
    kv_ks( T, R ).
