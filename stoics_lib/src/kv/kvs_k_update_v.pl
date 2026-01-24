/** kvs_k_update_v( +KVset, +K, +GoalNull, GoalUpd, -V, -NewV, -NVset ).

Update or insert the V part of two arg sorted terms list KVset to NVset.

GoalNull is called if K does not appear in KVset and GoalUpd when K does exist.
First is via =| call(+GoalNull, +K, -NewV, -NewKV)|= and second via =| call( +GoalUpd, +K, +V, -NewV, -NewKV ).
In the former case, V remain unbound.

==  
?- assert( v_plus_1(K,V,NewV,K-NewV):-NewV is V + 1).

?- v_plus_1( a, 2, Nv, KV ).
   Nv = 3,
   KV = a-3.

?- assert( v_plus(Plus,K,V,NewV,K-NewV):-NewV is V + Plus).

?- v_plus( 2, a, 2, Nv, KV ).
   Nv = 4,
   KV = a-4.

?- assert( v_val(Val,K,Val,K-Val) ).

?- v_val( 1, a, Val, KV ).
   Val = 1,
   KV = a-1.


?- kvs_k_update_v( [a-1,b-2,c-2], b, v_val(1), v_plus(1), V, Nv, Set ).
V = 2,
Nv = 3,
Set = [a-1, b-3, c-2].

105 ?- kvs_k_update_v( [a-1,b-2,c-2], d, v_val(0), v_plus(1), V, Nv, Set ).
Nv = 0,
Set = [a-1, b-2, c-2, d-0].

103 ?- kvs_k_update_v( [a-1,b-2,c-2], 0, v_val(1), v_plus(1), V, Nv, Set ).
Nv = 1,
Set = [0-1, a-1, b-2, c-2].
==

The predicate also works for any 2 arg terms as the Goals are responsible for 
constructing the precise form.

==
?- assert( v_plus_plus_kv(Plus,K,V,NewV,K+NewV):-NewV is V + Plus).
?- assert( v_val_plus_kv(Val,K,Val,K+Val) ).

?- kvs_k_update_v( [a+1,b+2,c+2], b, v_val_plus_kv(1), v_plus_plus_kv(1), V, Nv, Set ).
V = 2,
Nv = 3,
Set = [a+1, b+3, c+2].
==

@author  nicos angelopoulos
@version 0.1 2026/1/24

*/
kvs_k_update_v( [], K, Noal, _Uoal, _V, NewV, NVset ) :- 
     call( Noal, K, NewV, NewKV ),
     NVset = [NewKV].
kvs_k_update_v( [KV|KVset], K, Noal, Uoal, V, NewV, NVset ) :-
     arg( 1, KV, K1 ),
     compare( Ord, K1, K ),
     kvs_k_update_v_ord( Ord, KV, KVset, K, Noal, Uoal, V, NewV, NVset ).

kvs_k_update_v_ord( =, KV, KVset, K, _Noal, Uoal, V, NewV, NVset ) :-
     arg( 2, KV, V ),
     call( Uoal, K, V, NewV, NewKV ),
     NVset = [NewKV|KVset].
kvs_k_update_v_ord( <, KV, KVset, K, Noal, Uoal, V, NewV, NVset ) :-
     NVset = [KV|RemNVset],
     kvs_k_update_v( KVset, K, Noal, Uoal, V, NewV, RemNVset ).
kvs_k_update_v_ord( >, KV, KVset, K, Noal, _Uoal, _V, NewV, NVset ) :-
     call( Noal, K, NewV, NewKV ),
     NVset = [NewKV,KV|KVset].
