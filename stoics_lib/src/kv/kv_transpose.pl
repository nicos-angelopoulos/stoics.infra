
kv_transpose_defaults([]).

/** kv_transpose( +KVs, -VKs ).

Transopose the elements of KV pairs.

Examples
==
?- kv_transpose( [a-3,b-5], Trans ).
Trans = [3-a, 5-b].
==

@author nicos angelopoulos
@version  0.1 2023/08/30

*/

kv_transpose( [], [] ).
kv_transpose( [K-V|T], [V-K|R] ) :-
     kv_transpose( T, R ).
